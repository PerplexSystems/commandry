(*
 * SPDX-FileCopyrightText: 2025 Victor Freire <https://gluer.org>
 *
 * SPDX-License-Identifier: MIT
 *)

structure CommandryTests =
struct
  open Railroad
  structure T = Test
  structure E = Expect

  infix |>
  fun x |> f = f x

  fun listCompare cmp ([], []) = EQUAL
    | listCompare _ ([], _ :: _) = LESS
    | listCompare _ (_ :: _, []) = GREATER
    | listCompare cmp (x :: xs, y :: ys) =
        case cmp (x, y) of
          EQUAL => listCompare cmp (xs, ys)
        | ord => ord

  fun isOk result =
    case result of
      Cli.Ok _ => true
    | Cli.Err _ => false

  fun isErr result =
    case result of
      Cli.Ok _ => false
    | Cli.Err _ => true

  fun getOk result =
    case result of
      Cli.Ok (Cli.Command v) => SOME v
    | _ => NONE

  fun getErr result =
    case result of
      Cli.Ok _ => NONE
    | Cli.Err e => SOME e

  val flagParser =
    Cli.app "test" "1.0.0" "Test app" (fn v => v)|> Cli.flag
      {long = "verbose", short = SOME #"v", help = "Enable verbose"}

  val flagTests = T.describe "Flags"
    [ T.test "parses long flag --verbose" (fn () =>
        let
          val result = Cli.parse (Cli.cli flagParser) ["--verbose"]
        in
          case getOk result of
            SOME v => E.isTrue v
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "parses short flag -v" (fn () =>
        let
          val result = Cli.parse (Cli.cli flagParser) ["-v"]
        in
          case getOk result of
            SOME v => E.isTrue v
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "flag defaults to false when not provided" (fn () =>
        let
          val result = Cli.parse (Cli.cli flagParser) []
        in
          case getOk result of
            SOME v => E.isFalse v
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "parses long-only flag" (fn () =>
        let
          val parser =
            Cli.app "test" "1.0.0" "Test" (fn d => d) |> Cli.flag
              {long = "debug", short = NONE, help = "Debug mode"}
          val result = Cli.parse (Cli.cli parser) ["--debug"]
        in
          case getOk result of
            SOME v => E.isTrue v
          | NONE => E.fail "Expected Ok result"
        end)
    ]

  val optionParser =
    Cli.app "test" "1.0.0" "Test app" (fn out => out) |> Cli.option
      { long = "output"
      , short = SOME #"o"
      , help = "Output file"
      , placeholder = "FILE"
      , default = "out.txt"
      }

  val optionTests = T.describe "String Options"
    [ T.test "parses long option --output value" (fn () =>
        let
          val result = Cli.parse (Cli.cli optionParser) ["--output", "result.txt"]
        in
          case getOk result of
            SOME v => E.equal String.compare "result.txt" v
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "parses short option -o value" (fn () =>
        let
          val result = Cli.parse (Cli.cli optionParser) ["-o", "result.txt"]
        in
          case getOk result of
            SOME v => E.equal String.compare "result.txt" v
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "parses option with = syntax" (fn () =>
        let
          val result = Cli.parse (Cli.cli optionParser) ["--output=result.txt"]
        in
          case getOk result of
            SOME v => E.equal String.compare "result.txt" v
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "uses default when option not provided" (fn () =>
        let
          val result = Cli.parse (Cli.cli optionParser) []
        in
          case getOk result of
            SOME v => E.equal String.compare "out.txt" v
          | NONE => E.fail "Expected Ok result"
        end)
    ]

  val optionOptParser =
    Cli.app "test" "1.0.0" "Test app" (fn cfg => cfg) |> Cli.optionOpt
      { long = "config"
      , short = SOME #"c"
      , help = "Config file"
      , placeholder = "FILE"
      }

  val optionOptTests = T.describe "Optional String Options"
    [ T.test "parses optional option when provided" (fn () =>
        let
          val result = Cli.parse (Cli.cli optionOptParser) ["--config", "app.cfg"]
        in
          case getOk result of
            SOME (SOME v) => E.equal String.compare "app.cfg" v
          | SOME NONE => E.fail "Expected SOME value"
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "returns NONE when optional option not provided" (fn () =>
        let
          val result = Cli.parse (Cli.cli optionOptParser) []
        in
          case getOk result of
            SOME v => E.none v
          | NONE => E.fail "Expected Ok result"
        end)
    ]

  val intOptionParser =
    Cli.app "test" "1.0.0" "Test app" (fn n => n) |> Cli.optionInt
      { long = "count"
      , short = SOME #"n"
      , help = "Count"
      , placeholder = "NUM"
      , default = 1
      }

  val intOptionTests = T.describe "Integer Options"
    [ T.test "parses integer option" (fn () =>
        let
          val result = Cli.parse (Cli.cli intOptionParser) ["--count", "42"]
        in
          case getOk result of
            SOME v => E.equal Int.compare 42 v
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "parses negative integer" (fn () =>
        let
          val result = Cli.parse (Cli.cli intOptionParser) ["-n", "~5"]
        in
          case getOk result of
            SOME v => E.equal Int.compare ~5 v
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "uses default when not provided" (fn () =>
        let
          val result = Cli.parse (Cli.cli intOptionParser) []
        in
          case getOk result of
            SOME v => E.equal Int.compare 1 v
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "returns error for invalid integer" (fn () =>
        let
          val result = Cli.parse (Cli.cli intOptionParser) ["--count", "abc"]
        in
          case getErr result of
            SOME (Cli.InvalidValue _) => E.pass
          | _ => E.fail "Expected InvalidValue error"
        end)
    ]

  val intOptionOptParser =
    Cli.app "test" "1.0.0" "Test app" (fn n => n) |> Cli.optionIntOpt
      {long = "port", short = SOME #"p", help = "Port", placeholder = "NUM"}

  val intOptionOptTests = T.describe "Optional Integer Options"
    [ T.test "parses optional integer when provided" (fn () =>
        let
          val result = Cli.parse (Cli.cli intOptionOptParser) ["--port", "8080"]
        in
          case getOk result of
            SOME (SOME v) => E.equal Int.compare 8080 v
          | SOME NONE => E.fail "Expected SOME value"
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "returns NONE when not provided" (fn () =>
        let
          val result = Cli.parse (Cli.cli intOptionOptParser) []
        in
          case getOk result of
            SOME v => E.none v
          | NONE => E.fail "Expected Ok result"
        end)
    ]

  val positionalParser =
    Cli.app "test" "1.0.0" "Test app" (fn f => f) |> Cli.positional
      {name = "file", help = "Input file"}

  val positionalTests = T.describe "Positional Arguments"
    [ T.test "parses required positional argument" (fn () =>
        let
          val result = Cli.parse (Cli.cli positionalParser) ["input.txt"]
        in
          case getOk result of
            SOME v => E.equal String.compare "input.txt" v
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "shows help when no args and required positional" (fn () =>
        let
          val result = Cli.parse (Cli.cli positionalParser) []
        in
          case result of
            Cli.Ok (Cli.Help _) => E.pass
          | _ => E.fail "Expected Help when no args given"
        end)
    ]

  val positionalOptParser =
    Cli.app "test" "1.0.0" "Test app" (fn f => f) |> Cli.positionalOpt
      {name = "file", help = "Optional input file"}

  val positionalOptTests = T.describe "Optional Positional Arguments"
    [ T.test "parses optional positional when provided" (fn () =>
        let
          val result = Cli.parse (Cli.cli positionalOptParser) ["input.txt"]
        in
          case getOk result of
            SOME (SOME v) => E.equal String.compare "input.txt" v
          | SOME NONE => E.fail "Expected SOME value"
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "returns NONE when optional positional not provided" (fn () =>
        let
          val result = Cli.parse (Cli.cli positionalOptParser) []
        in
          case getOk result of
            SOME v => E.none v
          | NONE => E.fail "Expected Ok result"
        end)
    ]

  val positionalListParser =
    Cli.app "test" "1.0.0" "Test app" (fn fs => fs) |> Cli.positionalList
      {name = "files", help = "Input files"}

  val positionalListTests = T.describe "Positional List Arguments"
    [ T.test "parses multiple positional arguments" (fn () =>
        let
          val result =
            Cli.parse (Cli.cli positionalListParser) ["a.txt", "b.txt", "c.txt"]
        in
          case getOk result of
            SOME v =>
              E.equal (listCompare String.compare) ["a.txt", "b.txt", "c.txt"] v
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "returns empty list when no positionals provided" (fn () =>
        let
          val result = Cli.parse (Cli.cli positionalListParser) []
        in
          case getOk result of
            SOME v => E.equal (listCompare String.compare) [] v
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "parses single positional into list" (fn () =>
        let
          val result = Cli.parse (Cli.cli positionalListParser) ["only.txt"]
        in
          case getOk result of
            SOME v => E.equal (listCompare String.compare) ["only.txt"] v
          | NONE => E.fail "Expected Ok result"
        end)
    ]

  type config = {verbose: bool, output: string, count: int, input: string}

  val combinedParser =
    Cli.app "test" "1.0.0" "Test app"
      (fn v =>
         fn out =>
           fn cnt =>
             fn inp => {verbose = v, output = out, count = cnt, input = inp}) |>
      Cli.flag {long = "verbose", short = SOME #"v", help = "Verbose"} |>
      Cli.option
      { long = "output"
      , short = SOME #"o"
      , help = "Output"
      , placeholder = "FILE"
      , default = "out.txt"
      } |> Cli.optionInt
      { long = "count"
      , short = SOME #"n"
      , help = "Count"
      , placeholder = "NUM"
      , default = 1
      } |> Cli.positional {name = "input", help = "Input file"}
    |> Cli.cli

  val combinedTests = T.describe "Combined Parser"
    [ T.test "parses all arguments together" (fn () =>
        let
          val result = Cli.parse combinedParser
            ["-v", "--output", "result.txt", "-n", "5", "input.txt"]
        in
          case getOk result of
            SOME cfg =>
              if
                #verbose cfg andalso #output cfg = "result.txt"
                andalso #count cfg = 5 andalso #input cfg = "input.txt"
              then E.pass
              else E.fail "Config values don't match"
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "handles arguments in any order" (fn () =>
        let
          val result = Cli.parse combinedParser
            ["input.txt", "-n", "3", "-v", "-o", "out.bin"]
        in
          case getOk result of
            SOME cfg =>
              if
                #verbose cfg andalso #output cfg = "out.bin"
                andalso #count cfg = 3 andalso #input cfg = "input.txt"
              then E.pass
              else E.fail "Config values don't match"
          | NONE => E.fail "Expected Ok result"
        end)
    , T.test "uses defaults for unprovided options" (fn () =>
        let
          val result = Cli.parse combinedParser ["myfile.txt"]
        in
          case getOk result of
            SOME cfg =>
              if
                not (#verbose cfg) andalso #output cfg = "out.txt"
                andalso #count cfg = 1 andalso #input cfg = "myfile.txt"
              then E.pass
              else E.fail "Default values don't match"
          | NONE => E.fail "Expected Ok result"
        end)
    ]

  val errorTests = T.describe "Error Handling"
    [ T.test "returns UnknownOption for unrecognized flag" (fn () =>
        let
          val result = Cli.parse (Cli.cli flagParser) ["--unknown"]
        in
          case getErr result of
            SOME (Cli.UnknownOption _) => E.pass
          | _ => E.fail "Expected UnknownOption error"
        end)
    , T.test "returns MissingValue when option value missing" (fn () =>
        let
          val result = Cli.parse (Cli.cli optionParser) ["--output"]
        in
          case getErr result of
            SOME (Cli.MissingValue _) => E.pass
          | _ => E.fail "Expected MissingValue error"
        end)
    , T.test "returns Help for --help" (fn () =>
        let
          val result = Cli.parse (Cli.cli flagParser) ["--help"]
        in
          case result of
            Cli.Ok (Cli.Help _) => E.pass
          | _ => E.fail "Expected Help"
        end)
    , T.test "returns Help for -h" (fn () =>
        let
          val result = Cli.parse (Cli.cli flagParser) ["-h"]
        in
          case result of
            Cli.Ok (Cli.Help _) => E.pass
          | _ => E.fail "Expected Help"
        end)
    , T.test "returns Version for --version" (fn () =>
        let
          val result = Cli.parse (Cli.cli flagParser) ["--version"]
        in
          case result of
            Cli.Ok (Cli.Version _) => E.pass
          | _ => E.fail "Expected Version"
        end)
    , T.test "returns Version for -V" (fn () =>
        let
          val result = Cli.parse (Cli.cli flagParser) ["-V"]
        in
          case result of
            Cli.Ok (Cli.Version _) => E.pass
          | _ => E.fail "Expected Version"
        end)
    ]

  datatype cmd = Add of string list | Status of bool

  val addCmd =
    Cli.app "add" "" "Add files" (fn files => Add files) |> Cli.positionalList
      {name = "files", help = "Files to add"}

  val statusCmd =
    Cli.app "status" "" "Show status" (fn short => Status short) |> Cli.flag
      {long = "short", short = SOME #"s", help = "Short format"}

  val mainCli =
    Cli.app "mygit" "1.0.0" "Git clone" (Status false) 
    |> Cli.cli |>
      Cli.subcommand {name = "add", help = "Add files", cmd = addCmd} |>
      Cli.subcommand
      {name = "status", help = "Show status", cmd = statusCmd}

  val subcommandTests = T.describe "Subcommands"
    [ T.test "parses add subcommand" (fn () =>
        let
          val result = Cli.parse mainCli ["add", "file1.txt", "file2.txt"]
        in
          case result of
            Cli.Ok (Cli.Command (Add files)) =>
              E.equal (listCompare String.compare) ["file1.txt", "file2.txt"]
                files
          | _ => E.fail "Expected Add command"
        end)
    , T.test "parses status subcommand with flag" (fn () =>
        let
          val result = Cli.parse mainCli ["status", "-s"]
        in
          case result of
            Cli.Ok (Cli.Command (Status short)) => E.isTrue short
          | _ => E.fail "Expected Status command"
        end)
    , T.test "parses status subcommand without flag" (fn () =>
        let
          val result = Cli.parse mainCli ["status"]
        in
          case result of
            Cli.Ok (Cli.Command (Status short)) => E.isFalse short
          | _ => E.fail "Expected Status command"
        end)
    ]

  val helpTests = T.describe "Help Text"
    [ T.test "help returns non-empty string" (fn () =>
        let val helpText = Cli.help (Cli.cli flagParser)
        in E.isTrue (String.size helpText > 0)
        end)
    , T.test "help includes app name" (fn () =>
        let val helpText = Cli.help mainCli
        in E.isTrue (String.isSubstring "mygit" helpText)
        end)
    , T.test "help includes version" (fn () =>
        let val helpText = Cli.help mainCli
        in E.isTrue (String.isSubstring "1.0.0" helpText)
        end)
    , T.test "help includes subcommand names" (fn () =>
        let
          val helpText = Cli.help mainCli
        in
          if
            String.isSubstring "add" helpText
            andalso String.isSubstring "status" helpText
          then
            E.pass
          else
            E.fail
              "Expected help to include subcommand names 'add' and 'status'"
        end)
    , T.test "subcommand --help shows subcommand help" (fn () =>
        let
          val result = Cli.parse mainCli ["add", "--help"]
        in
          case result of
            Cli.Ok (Cli.Help helpText) =>
              if
                String.isSubstring "add" helpText
                andalso String.isSubstring "files" helpText
              then
                E.pass
              else
                E.fail "Expected subcommand help to include 'add' and 'files'"
          | _ => E.fail "Expected Help for subcommand --help"
        end)
    , T.test "errorToString produces readable message" (fn () =>
        let val errStr = Cli.errorToString (Cli.UnknownOption "--foo")
        in E.isTrue (String.size errStr > 0)
        end)
    ]

  val allTests = T.concat
    [ flagTests
    , optionTests
    , optionOptTests
    , intOptionTests
    , intOptionOptTests
    , positionalTests
    , positionalOptTests
    , positionalListTests
    , combinedTests
    , errorTests
    , subcommandTests
    , helpTests
    ]

  val _ = T.run allTests
end
