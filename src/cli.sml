(*
 * SPDX-FileCopyrightText: 2025 Victor Freire <https://gluer.org>
 *
 * SPDX-License-Identifier: MIT
 *)

structure Cli :>
sig
  datatype ('a, 'e) result = Ok of 'a | Err of 'e

  datatype 'a command = Command of 'a | Help of string | Version of string

  datatype error =
    UnknownOption of string
  | MissingValue of string
  | InvalidValue of {name: string, value: string, expected: string}
  | MissingRequired of string

  type 'a parser

  (* a finalized parser that can have subcommands *)
  type 'a cli

  val app: string -> string -> string -> 'a -> 'a parser
  
  val flag: {long: string, short: char option, help: string}
            -> (bool -> 'a) parser
            -> 'a parser

  val option:
    { long: string
    , short: char option
    , help: string
    , placeholder: string
    , default: string
    }
    -> (string -> 'a) parser
    -> 'a parser

  val optionOpt:
    {long: string, short: char option, help: string, placeholder: string}
    -> (string option -> 'a) parser
    -> 'a parser

  val optionInt:
    { long: string
    , short: char option
    , help: string
    , placeholder: string
    , default: int
    }
    -> (int -> 'a) parser
    -> 'a parser

  val optionIntOpt:
    {long: string, short: char option, help: string, placeholder: string}
    -> (int option -> 'a) parser
    -> 'a parser

  val positional: {name: string, help: string}
                  -> (string -> 'a) parser
                  -> 'a parser

  val positionalOpt: {name: string, help: string}
                     -> (string option -> 'a) parser
                     -> 'a parser

  val positionalList: {name: string, help: string}
                      -> (string list -> 'a) parser
                      -> 'a parser

  (* Convert a parser to a CLI (required before adding subcommands or running) *)
  val cli: 'a parser -> 'a cli

  (* Add a subcommand. All subcommands must produce the same type. *)
  val subcommand: {name: string, help: string, cmd: 'a parser}
                  -> 'a cli
                  -> 'a cli

  (* Run a CLI *)
  val parse: 'a cli -> string list -> ('a command, error) result

  (* Main entry point - handles help, version, errors, and exit codes *)
  val exec: ('a -> unit) -> 'a cli -> unit

  (* Get help text *)
  val help: 'a cli -> string
  val parserHelp: 'a parser -> string

  val errorToString: error -> string

  (* Print message to stderr and exit with failure *)
  val die: string -> 'a

end =
struct

  datatype ('a, 'e) result = Ok of 'a | Err of 'e

  datatype 'a command = Command of 'a | Help of string | Version of string

  datatype error =
    UnknownOption of string
  | MissingValue of string
  | InvalidValue of {name: string, value: string, expected: string}
  | MissingRequired of string

  datatype value =
    VBool of bool
  | VString of string
  | VStringOpt of string option
  | VInt of int
  | VIntOpt of int option
  | VStringList of string list

  datatype arg_kind =
    KFlag
  | KOption of {placeholder: string, default: string option}
  | KOptionInt of {placeholder: string, default: int option}
  | KPositional of {required: bool, multiple: bool}

  type arg_spec =
    {long: string, short: char option, help: string, kind: arg_kind}

  (* Parser type - for building with combinators (no subcommands) *)
  datatype 'a parser =
    Parser of
      { name: string
      , version: string
      , description: string
      , args: arg_spec list
      , build: value list -> 'a
      }

  (* CLI type - finalized parser with optional subcommands *)
  type 'a runner = string list -> ('a command, error) result

  datatype 'a cli =
    Cli of
      { name: string
      , version: string
      , description: string
      , args: arg_spec list
      , subcommands: (string * string * 'a runner) list
      , build: value list -> 'a
      }

  fun isOption s =
    String.size s > 0 andalso String.sub (s, 0) = #"-"

  fun splitEq s =
    case CharVector.findi (fn (_, c) => c = #"=") s of
      NONE => NONE
    | SOME (i, _) =>
        SOME (String.substring (s, 0, i), String.substring
          (s, i + 1, String.size s - i - 1))

  datatype token =
    TFlag of string
  | TOption of string * string
  | TPositional of string
  | TDoubleDash
  | THelp
  | TVersion

  (* Internal result type for parseTokens to signal help/version requests *)
  datatype tokensResult =
    TokensOk of (string * value) list * string list
  | TokensHelp
  | TokensVersion
  | TokensErr of error

  fun tokenize (args: string list) : token list =
    let
      fun go [] acc = rev acc
        | go ("--help" :: rest) acc =
            go rest (THelp :: acc)
        | go ("-h" :: rest) acc =
            go rest (THelp :: acc)
        | go ("--version" :: rest) acc =
            go rest (TVersion :: acc)
        | go ("-V" :: rest) acc =
            go rest (TVersion :: acc)
        | go ("--" :: rest) acc =
            rev acc @ TDoubleDash :: map TPositional rest
        | go (arg :: rest) acc =
            if String.isPrefix "--" arg then
              case splitEq arg of
                SOME (opt, value) => go rest (TOption (opt, value) :: acc)
              | NONE =>
                  (case rest of
                     [] => go [] (TFlag arg :: acc)
                   | (next :: rest2) =>
                       if isOption next then go rest (TFlag arg :: acc)
                       else go rest2 (TOption (arg, next) :: acc))
            else if String.isPrefix "-" arg andalso String.size arg = 2 then
              case rest of
                [] => go [] (TFlag arg :: acc)
              | (next :: rest2) =>
                  if isOption next then go rest (TFlag arg :: acc)
                  else go rest2 (TOption (arg, next) :: acc)
            else if String.isPrefix "-" arg andalso String.size arg > 2 then
              case splitEq arg of
                SOME (opt, value) => go rest (TOption (opt, value) :: acc)
              | NONE =>
                  let
                    val chars = String.explode (String.extract (arg, 1, NONE))
                    val flags = map (fn c => TFlag ("-" ^ String.str c)) chars
                  in
                    go rest (rev flags @ acc)
                  end
            else
              go rest (TPositional arg :: acc)
    in
      go args []
    end

  fun parseTokens (argSpecs: arg_spec list) (tokens: token list) : tokensResult =
    let
      fun findSpec name =
        List.find
          (fn {long, short, ...} =>
             "--" ^ long = name
             orelse
             (case short of
                SOME c => "-" ^ String.str c = name
              | NONE => false)) argSpecs

      fun go [] values positionals =
            TokensOk (rev values, rev positionals)
        | go (THelp :: _) _ _ = TokensHelp
        | go (TVersion :: _) _ _ = TokensVersion
        | go (TDoubleDash :: rest) values positionals =
            let
              val posArgs =
                List.mapPartial (fn TPositional s => SOME s | _ => NONE) rest
            in
              TokensOk (rev values, rev positionals @ posArgs)
            end
        | go (TPositional s :: rest) values positionals =
            go rest values (s :: positionals)
        | go (TFlag name :: rest) values positionals =
            (case findSpec name of
               NONE => TokensErr (UnknownOption name)
             | SOME {long, kind = KFlag, ...} =>
                 go rest ((long, VBool true) :: values) positionals
             | SOME {long, ...} => TokensErr (MissingValue long))
        | go (TOption (name, value) :: rest) values positionals =
            (case findSpec name of
               NONE => TokensErr (UnknownOption name)
             | SOME {long, kind = KFlag, ...} =>
                 go rest ((long, VBool true) :: values) (value :: positionals)
             | SOME {long, kind = KOption _, ...} =>
                 go rest ((long, VString value) :: values) positionals
             | SOME {long, kind = KOptionInt _, ...} =>
                 (case Int.fromString value of
                    NONE =>
                      TokensErr
                        (InvalidValue
                           {name = long, value = value, expected = "integer"})
                  | SOME n => go rest ((long, VInt n) :: values) positionals)
             | SOME {long, kind = KPositional _, ...} =>
                 go rest ((long, VString value) :: values) positionals)
    in
      go tokens [] []
    end

  fun app name version description f =
    Parser
      { name = name
      , version = version
      , description = description
      , args = []
      , build = fn [] => f | _ => raise Fail "app: expected no values"
      }

  fun flag {long, short, help} (Parser p) =
    let
      val spec = {long = long, short = short, help = help, kind = KFlag}
      val newBuild = fn values =>
        let
          val (boolVal, rest) =
            case values of
              (VBool b :: r) => (b, r)
            | _ => (false, values)
        in
          (#build p rest) boolVal
        end
    in
      Parser
        { name = #name p
        , version = #version p
        , description = #description p
        , args = spec :: #args p
        , build = newBuild
        }
    end

  fun option {long, short, help, placeholder, default} (Parser p) =
    let
      val spec =
        { long = long
        , short = short
        , help = help
        , kind = KOption {placeholder = placeholder, default = SOME default}
        }
      val newBuild = fn values =>
        let
          val (strVal, rest) =
            case values of
              (VString s :: r) => (s, r)
            | _ => (default, values)
        in
          (#build p rest) strVal
        end
    in
      Parser
        { name = #name p
        , version = #version p
        , description = #description p
        , args = spec :: #args p
        , build = newBuild
        }
    end

  fun optionOpt {long, short, help, placeholder} (Parser p) =
    let
      val spec =
        { long = long
        , short = short
        , help = help
        , kind = KOption {placeholder = placeholder, default = NONE}
        }
      val newBuild = fn values =>
        let
          val (optVal, rest) =
            case values of
              (VString s :: r) => (SOME s, r)
            | (VStringOpt s :: r) => (s, r)
            | _ => (NONE, values)
        in
          (#build p rest) optVal
        end
    in
      Parser
        { name = #name p
        , version = #version p
        , description = #description p
        , args = spec :: #args p
        , build = newBuild
        }
    end

  fun optionInt {long, short, help, placeholder, default} (Parser p) =
    let
      val spec =
        { long = long
        , short = short
        , help = help
        , kind = KOptionInt {placeholder = placeholder, default = SOME default}
        }
      val newBuild = fn values =>
        let
          val (intVal, rest) =
            case values of
              (VInt n :: r) => (n, r)
            | _ => (default, values)
        in
          (#build p rest) intVal
        end
    in
      Parser
        { name = #name p
        , version = #version p
        , description = #description p
        , args = spec :: #args p
        , build = newBuild
        }
    end

  fun optionIntOpt {long, short, help, placeholder} (Parser p) =
    let
      val spec =
        { long = long
        , short = short
        , help = help
        , kind = KOptionInt {placeholder = placeholder, default = NONE}
        }
      val newBuild = fn values =>
        let
          val (optVal, rest) =
            case values of
              (VInt n :: r) => (SOME n, r)
            | (VIntOpt n :: r) => (n, r)
            | _ => (NONE, values)
        in
          (#build p rest) optVal
        end
    in
      Parser
        { name = #name p
        , version = #version p
        , description = #description p
        , args = spec :: #args p
        , build = newBuild
        }
    end

  fun positional {name, help} (Parser p) =
    let
      val spec =
        { long = name
        , short = NONE
        , help = help
        , kind = KPositional {required = true, multiple = false}
        }
      val newBuild = fn values =>
        let
          val (strVal, rest) =
            case values of
              (VString s :: r) => (s, r)
            | _ => raise Fail name
        in
          (#build p rest) strVal
        end
    in
      Parser
        { name = #name p
        , version = #version p
        , description = #description p
        , args = spec :: #args p
        , build = newBuild
        }
    end

  fun positionalOpt {name, help} (Parser p) =
    let
      val spec =
        { long = name
        , short = NONE
        , help = help
        , kind = KPositional {required = false, multiple = false}
        }
      val newBuild = fn values =>
        let
          val (optVal, rest) =
            case values of
              (VString s :: r) => (SOME s, r)
            | (VStringOpt s :: r) => (s, r)
            | _ => (NONE, values)
        in
          (#build p rest) optVal
        end
    in
      Parser
        { name = #name p
        , version = #version p
        , description = #description p
        , args = spec :: #args p
        , build = newBuild
        }
    end

  fun positionalList {name, help} (Parser p) =
    let
      val spec =
        { long = name
        , short = NONE
        , help = help
        , kind = KPositional {required = false, multiple = true}
        }
      val newBuild = fn values =>
        let
          val (listVal, rest) =
            case values of
              (VStringList lst :: r) => (lst, r)
            | _ => ([], values)
        in
          (#build p rest) listVal
        end
    in
      Parser
        { name = #name p
        , version = #version p
        , description = #description p
        , args = spec :: #args p
        , build = newBuild
        }
    end

  (* Convert a parser to a CLI *)
  fun cli (Parser p) =
    Cli
      { name = #name p
      , version = #version p
      , description = #description p
      , args = #args p
      , subcommands = []
      , build = #build p
      }

  fun collectValues (argSpecs: arg_spec list) (parsed: (string * value) list)
    (positionals: string list) : (value list, error) result =
    let
      fun lookup name =
        List.find (fn (n, _) => n = name) parsed

      fun getPositionalSpecs specs =
        List.filter
          (fn {kind = KPositional _, help = _, long = _, short = _} => true
            | _ => false) specs

      fun getNonPositionalSpecs specs =
        List.filter
          (fn {kind = KPositional _, help = _, long = _, short = _} => false
            | _ => true) specs

      val posSpecs = getPositionalSpecs argSpecs
      val optSpecs = getNonPositionalSpecs argSpecs

      fun matchPositionals [] [] acc =
            Ok (rev acc)
        | matchPositionals [] (spec :: rest) acc =
            (case #kind spec of
               KPositional {required = true, multiple = _} =>
                 Err (MissingRequired (#long spec))
             | KPositional {required = false, multiple = true} =>
                 matchPositionals [] rest (VStringList [] :: acc)
             | KPositional {required = false, multiple = false} =>
                 matchPositionals [] rest (VStringOpt NONE :: acc)
             | _ => matchPositionals [] rest acc)
        | matchPositionals _ [] acc =
            Ok (rev acc)
        | matchPositionals (arg :: args) (spec :: rest) acc =
            (case #kind spec of
               KPositional {required = _, multiple = true} =>
                 matchPositionals [] rest (VStringList (arg :: args) :: acc)
             | KPositional {required = _, multiple = false} =>
                 matchPositionals args rest (VString arg :: acc)
             | _ => matchPositionals (arg :: args) rest acc)

      fun getOptValues [] acc =
            Ok (rev acc)
        | getOptValues (spec :: rest) acc =
            let
              val name = #long spec
            in
              case (#kind spec, lookup name) of
                (KFlag, SOME (_, v)) => getOptValues rest (v :: acc)
              | (KFlag, NONE) => getOptValues rest (VBool false :: acc)
              | (KOption {default = SOME d, ...}, NONE) =>
                  getOptValues rest (VString d :: acc)
              | (KOption _, SOME (_, v)) => getOptValues rest (v :: acc)
              | (KOption {default = NONE, ...}, NONE) =>
                  getOptValues rest (VStringOpt NONE :: acc)
              | (KOptionInt {default = SOME d, ...}, NONE) =>
                  getOptValues rest (VInt d :: acc)
              | (KOptionInt _, SOME (_, v)) => getOptValues rest (v :: acc)
              | (KOptionInt {default = NONE, ...}, NONE) =>
                  getOptValues rest (VIntOpt NONE :: acc)
              | (KPositional _, _) => getOptValues rest acc
            end
    in
      case getOptValues optSpecs [] of
        Err e => Err e
      | Ok optVals =>
          case matchPositionals positionals (rev posSpecs) [] of
            Err e => Err e
          (* Values must be in reverse spec order: last combinator first *)
          | Ok posVals => Ok (posVals @ rev optVals)
    end

  (* Internal help text generator *)
  fun helpImpl {name, version, description, argSpecs, subcommands} : string =
    let
      val hasSubcommands = not (null subcommands)

      fun formatShort NONE = "   "
        | formatShort (SOME c) =
            "-" ^ String.str c ^ ","

      fun formatLong long = "--" ^ long

      fun formatPlaceholder (KOption {placeholder, ...}) =
            " <" ^ placeholder ^ ">"
        | formatPlaceholder (KOptionInt {placeholder, ...}) =
            " <" ^ placeholder ^ ">"
        | formatPlaceholder _ = ""

      fun formatDefault (KOption {default = SOME d, ...}) =
            " [default: " ^ d ^ "]"
        | formatDefault (KOptionInt {default = SOME d, ...}) =
            " [default: " ^ Int.toString d ^ "]"
        | formatDefault _ = ""

      fun formatOpt {long, short, help, kind} =
        let
          val shortStr = formatShort short
          val longStr = formatLong long
          val placeholder = formatPlaceholder kind
          val default = formatDefault kind
          val optPart = "    " ^ shortStr ^ " " ^ longStr ^ placeholder
          val padding =
            CharVector.tabulate (Int.max (0, 28 - String.size optPart), fn _ =>
              #" ")
        in
          optPart ^ padding ^ help ^ default ^ "\n"
        end

      fun formatPositional
            {long, kind = KPositional {required, multiple}, help, short = _} =
            let
              val namePart =
                if required then "<" ^ long ^ ">" else "[" ^ long ^ "]"
              val nameFinal = if multiple then namePart ^ "..." else namePart
              val padding =
                CharVector.tabulate
                  (Int.max (0, 16 - String.size nameFinal), fn _ => #" ")
            in
              "    " ^ nameFinal ^ padding ^ help ^ "\n"
            end
        | formatPositional _ = ""

      fun formatSubcommand (cmdName, helpText, _) =
        let
          val padding =
            CharVector.tabulate (Int.max (0, 16 - String.size cmdName), fn _ =>
              #" ")
        in
          "    " ^ cmdName ^ padding ^ helpText ^ "\n"
        end

      val optSpecs =
        List.filter (fn {kind = KPositional _, ...} => false | _ => true)
          argSpecs
      val posSpecs =
        List.filter (fn {kind = KPositional _, ...} => true | _ => false)
          argSpecs

      val header = name ^ (if version = "" then "" else " " ^ version) ^ "\n"
      val desc = if description = "" then "" else description ^ "\n\n"

      val usage =
        let
          val opts = if null optSpecs then "" else "[OPTIONS]"
          val cmds = if hasSubcommands then "<COMMAND>" else ""
          val pos = String.concatWith " "
            (map
               (fn {long, kind = KPositional {required, multiple}, ...} =>
                  let
                    val n =
                      if required then "<" ^ long ^ ">" else "[" ^ long ^ "]"
                  in
                    if multiple then n ^ "..." else n
                  end
                 | _ => "") posSpecs)
        in
          "USAGE:\n    " ^ name ^ (if opts = "" then "" else " " ^ opts)
          ^ (if cmds = "" then "" else " " ^ cmds)
          ^ (if pos = "" then "" else " " ^ pos) ^ "\n\n"
        end

      val argsSection =
        if null posSpecs then ""
        else "ARGS:\n" ^ String.concat (map formatPositional posSpecs) ^ "\n"

      val commandsSection =
        if not hasSubcommands then
          ""
        else
          "COMMANDS:\n" ^ String.concat (map formatSubcommand subcommands)
          ^ "\n"

      val optsSection =
        "OPTIONS:\n" ^ String.concat (map formatOpt optSpecs)
        ^ "    -h, --help                  Print help information\n"
        ^ "    -V, --version               Print version information\n"
    in
      header ^ desc ^ usage ^ commandsSection ^ argsSection ^ optsSection
    end

  (* Help text for a parser (no subcommands) *)
  fun parserHelp (Parser p) : string =
    helpImpl
      { name = #name p
      , version = #version p
      , description = #description p
      , argSpecs = rev (#args p)
      , subcommands = []
      }

  (* Help text for a CLI (with subcommands) *)
  fun help (Cli c) : string =
    helpImpl
      { name = #name c
      , version = #version c
      , description = #description c
      , argSpecs = rev (#args c)
      , subcommands = rev (#subcommands c)
      }

  (* Internal helper for parsing without subcommand handling *)
  fun parseArgs (argSpecs: arg_spec list) (build: value list -> 'a)
    (helpText: string) (versionText: string) (args: string list) :
    ('a command, error) result =
    let
      val hasRequiredPositional =
        List.exists
          (fn {kind = KPositional {required = true, ...}, ...} => true
            | _ => false) argSpecs
      val tokens = tokenize args
    in
      if null args andalso hasRequiredPositional then
        Ok (Help helpText)
      else
        case parseTokens argSpecs tokens of
          TokensHelp => Ok (Help helpText)
        | TokensVersion => Ok (Version versionText)
        | TokensErr e => Err e
        | TokensOk (parsed, positionals) =>
            case collectValues argSpecs parsed positionals of
              Err e => Err e
            | Ok values =>
                Ok (Command (build values))
                handle Fail msg => Err (MissingRequired msg)
    end

  (* Parse a CLI (handles subcommands) *)
  and parse (Cli c) (args: string list) : ('a command, error) result =
    let
      val argSpecs = rev (#args c)
      val subcommands = #subcommands c
      val hasSubcommands = not (null subcommands)
      val helpText = help (Cli c)
      val versionText = #name c ^ " " ^ #version c ^ "\n"

      (* Find a subcommand by name *)
      fun findSubcommand name =
        List.find (fn (n, _, _) => n = name) subcommands

      (* Find first non-option argument that could be a subcommand *)
      fun findSubcommandArg [] = NONE
        | findSubcommandArg (arg :: rest) =
            if arg = "--help" orelse arg = "-h" then
              NONE
            else if arg = "--version" orelse arg = "-V" then
              NONE
            else if arg = "--" then
              NONE
            else if isOption arg then
              findSubcommandArg rest
            else
              case findSubcommand arg of
                SOME (_, _, runner) => SOME (arg, rest, runner)
              | NONE => NONE

      val tokens = tokenize args
    in
      if hasSubcommands then
        case findSubcommandArg args of
          SOME (_, subArgs, runner) =>
            (* Subcommand result passes through - it has its own help text *)
            runner subArgs
        | NONE =>
            (case tokens of
               (THelp :: _) => Ok (Help helpText)
             | (TVersion :: _) => Ok (Version versionText)
             | _ =>
                 if null args then
                   Ok (Help helpText)
                 else
                   case args of
                     [] => Ok (Help helpText)
                   | (first :: _) =>
                       if isOption first then Ok (Help helpText)
                       else Err (UnknownOption first))
      else
        parseArgs argSpecs (#build c) helpText versionText args
    end

  (* Add a subcommand to a CLI *)
  fun subcommand {name, help, cmd} (Cli c) =
    let
      fun runner args =
        parse (cli cmd) args
    in
      Cli
        { name = #name c
        , version = #version c
        , description = #description c
        , args = #args c
        , subcommands = (name, help, runner) :: #subcommands c
        , build = #build c
        }
    end

  fun errorToString (UnknownOption s) = "error: unknown option '" ^ s ^ "'"
    | errorToString (MissingValue s) =
        "error: option '" ^ s ^ "' requires a value"
    | errorToString (InvalidValue {name, value, expected}) =
        "error: invalid value '" ^ value ^ "' for '" ^ name ^ "': expected "
        ^ expected
    | errorToString (MissingRequired s) =
        "error: missing required argument: " ^ s

  fun die msg =
    ( TextIO.output (TextIO.stdErr, msg ^ "\n")
    ; OS.Process.exit OS.Process.failure
    )

  fun exec handler cli =
    case parse cli (CommandLine.arguments ()) of
      Ok (Command config) => handler config
    | Ok (Help text) => print text
    | Ok (Version text) => print text
    | Err e => die (errorToString e)

end
