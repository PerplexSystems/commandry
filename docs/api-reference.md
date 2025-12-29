# API Reference

The `Cli` structure provides a combinator-style API for building command-line argument parsers.

## Types

### Result Type

```sml
datatype ('a, 'e) result = Ok of 'a | Err of 'e
```

Standard result type for representing success or failure.

### Command Type

```sml
datatype 'a command = Command of 'a | Help of string | Version of string
```

Represents the outcome of parsing:
- `Command config` - Successfully parsed arguments into a configuration value
- `Help text` - User requested help (`-h` or `--help`)
- `Version text` - User requested version (`-V` or `--version`)

### Error Type

```sml
datatype error =
    UnknownOption of string
  | MissingValue of string
  | InvalidValue of {name: string, value: string, expected: string}
  | MissingRequired of string
```

Parse errors:
- `UnknownOption` - Unrecognized flag or option
- `MissingValue` - Option provided without a value
- `InvalidValue` - Value doesn't match expected type (e.g., non-integer for int option)
- `MissingRequired` - Required positional argument missing

### Parser Types

```sml
type 'a parser   (* A parser being built with combinators *)
type 'a cli      (* A finalized parser that can have subcommands *)
```

## Core Functions

### app

```sml
val app : string -> string -> string -> 'a -> 'a parser
```

Create a new CLI application parser.

**Parameters:**
- `name` - Application name (shown in help text)
- `version` - Version string (shown with `--version`)
- `description` - Short description (shown in help text)
- `initFn` - Initial value or curried function to receive parsed arguments

**Example:**
```sml
val parser = Cli.app "myapp" "1.0.0" "A sample application"
  (fn verbose => fn output => {verbose = verbose, output = output})
```

### cli

```sml
val cli : 'a parser -> 'a cli
```

Convert a parser to a CLI. Required before adding subcommands or running.

**Example:**
```sml
val myCli = Cli.cli parser
```

### parse

```sml
val parse : 'a cli -> string list -> ('a command, error) result
```

Parse command-line arguments, returning a result for custom handling.

**Example:**
```sml
case Cli.parse myCli ["--verbose", "output.txt"] of
    Cli.Ok (Cli.Command config) => (* use config *)
  | Cli.Ok (Cli.Help text) => print text
  | Cli.Ok (Cli.Version text) => print text
  | Cli.Err e => Cli.die (Cli.errorToString e ^ "\n")
```

### exec

```sml
val exec : ('a -> unit) -> 'a cli -> unit
```

Main entry point that handles help, version, errors, and exit codes automatically. Reads arguments from `CommandLine.arguments()`.

**Example:**
```sml
val () = Cli.exec (fn config => (* use config *)) myCli
```

## Argument Combinators

All combinators use the pipe operator `|>` to chain onto a parser.

### flag

```sml
val flag : {long: string, short: char option, help: string}
         -> (bool -> 'a) parser
         -> 'a parser
```

Add a boolean flag. Defaults to `false` when not provided.

**Parameters:**
- `long` - Long option name (used as `--long`)
- `short` - Optional short option character (used as `-c`)
- `help` - Help text description

**Example:**
```sml
Cli.app "myapp" "1.0.0" "desc" (fn v => v)
|> Cli.flag {long = "verbose", short = SOME #"v", help = "Enable verbose output"}
```

### option

```sml
val option : {long: string, short: char option, help: string,
              placeholder: string, default: string}
           -> (string -> 'a) parser
           -> 'a parser
```

Add a string option with a default value.

**Parameters:**
- `long` - Long option name
- `short` - Optional short option character
- `help` - Help text description
- `placeholder` - Placeholder shown in help (e.g., `FILE`)
- `default` - Default value when not provided

**Example:**
```sml
|> Cli.option {long = "output", short = SOME #"o", help = "Output file",
               placeholder = "FILE", default = "out.txt"}
```

### optionOpt

```sml
val optionOpt : {long: string, short: char option, help: string,
                 placeholder: string}
              -> (string option -> 'a) parser
              -> 'a parser
```

Add an optional string option. Returns `NONE` when not provided.

**Example:**
```sml
|> Cli.optionOpt {long = "config", short = SOME #"c",
                  help = "Config file", placeholder = "FILE"}
```

### optionInt

```sml
val optionInt : {long: string, short: char option, help: string,
                 placeholder: string, default: int}
              -> (int -> 'a) parser
              -> 'a parser
```

Add an integer option with a default value. Returns `InvalidValue` error for non-integer input.

**Example:**
```sml
|> Cli.optionInt {long = "count", short = SOME #"n", help = "Number of items",
                  placeholder = "NUM", default = 10}
```

### optionIntOpt

```sml
val optionIntOpt : {long: string, short: char option, help: string,
                    placeholder: string}
                 -> (int option -> 'a) parser
                 -> 'a parser
```

Add an optional integer option. Returns `NONE` when not provided.

**Example:**
```sml
|> Cli.optionIntOpt {long = "port", short = SOME #"p",
                     help = "Port number", placeholder = "PORT"}
```

### positional

```sml
val positional : {name: string, help: string}
               -> (string -> 'a) parser
               -> 'a parser
```

Add a required positional argument. If not provided, shows help text automatically.

**Parameters:**
- `name` - Argument name (shown in help as `<name>`)
- `help` - Help text description

**Example:**
```sml
|> Cli.positional {name = "input", help = "Input file to process"}
```

### positionalOpt

```sml
val positionalOpt : {name: string, help: string}
                  -> (string option -> 'a) parser
                  -> 'a parser
```

Add an optional positional argument. Returns `NONE` when not provided.

**Example:**
```sml
|> Cli.positionalOpt {name = "output", help = "Output file (optional)"}
```

### positionalList

```sml
val positionalList : {name: string, help: string}
                   -> (string list -> 'a) parser
                   -> 'a parser
```

Add a variadic positional argument that collects all remaining positional arguments into a list. Returns empty list when none provided.

**Example:**
```sml
|> Cli.positionalList {name = "files", help = "Input files"}
```

## Subcommands

### subcommand

```sml
val subcommand : {name: string, help: string, cmd: 'a parser}
               -> 'a cli
               -> 'a cli
```

Add a subcommand to a CLI. All subcommands must produce the same result type.

**Parameters:**
- `name` - Subcommand name
- `help` - Short description for help listing
- `cmd` - Parser for the subcommand

**Example:**
```sml
datatype cmd = Add of string list | Status of bool

val addCmd = Cli.app "add" "" "Add files" (fn files => Add files)
  |> Cli.positionalList {name = "files", help = "Files to add"}

val statusCmd = Cli.app "status" "" "Show status" (fn short => Status short)
  |> Cli.flag {long = "short", short = SOME #"s", help = "Short format"}

val myCli = Cli.app "mygit" "1.0.0" "Git clone" (Status false)
  |> Cli.cli
  |> Cli.subcommand {name = "add", help = "Add files to staging", cmd = addCmd}
  |> Cli.subcommand {name = "status", help = "Show repo status", cmd = statusCmd}
```

## Help Functions

### help

```sml
val help : 'a cli -> string
```

Generate help text for a CLI (includes subcommands if present).

### parserHelp

```sml
val parserHelp : 'a parser -> string
```

Generate help text for a parser (before conversion to CLI).

### errorToString

```sml
val errorToString : error -> string
```

Convert an error to a human-readable string.

**Example:**
```sml
Cli.errorToString (Cli.UnknownOption "--foo")
(* Returns: "error: unknown option '--foo'" *)
```

### die

```sml
val die : string -> 'a
```

Print a message to stderr and exit with failure status. Useful for error handling in CLI applications.

**Example:**
```sml
case Cli.parse myCli args of
    Cli.Ok (Cli.Command config) => runApp config
  | Cli.Ok (Cli.Help text) => print text
  | Cli.Ok (Cli.Version text) => print text
  | Cli.Err e => Cli.die (Cli.errorToString e ^ "\n")
```

## Argument Parsing Behavior

### Option Syntax

Options can be specified in multiple ways:
- Long form: `--option value` or `--option=value`
- Short form: `-o value` or `-o=value`
- Combined short flags: `-abc` expands to `-a -b -c`

### Special Arguments

- `--help` or `-h` - Shows help text
- `--version` or `-V` - Shows version
- `--` - Stops option parsing; remaining args are treated as positionals

### Argument Order

Options and positional arguments can be mixed in any order. The parser will correctly associate values with their respective options.

## Complete Example

```sml
type config = {
  verbose: bool,
  output: string,
  count: int,
  input: string
}

val parser =
  Cli.app "myapp" "1.0.0" "Process files with options"
    (fn verbose => fn output => fn count => fn input =>
      {verbose = verbose, output = output, count = count, input = input})
  |> Cli.flag {long = "verbose", short = SOME #"v", help = "Enable verbose output"}
  |> Cli.option {long = "output", short = SOME #"o", help = "Output file",
                 placeholder = "FILE", default = "out.txt"}
  |> Cli.optionInt {long = "count", short = SOME #"n", help = "Number of iterations",
                    placeholder = "NUM", default = 1}
  |> Cli.positional {name = "input", help = "Input file to process"}

val () = Cli.exec
  (fn cfg =>
    print ("Processing " ^ #input cfg ^
           " with count=" ^ Int.toString (#count cfg) ^ "\n"))
  (Cli.cli parser)
```

Usage:
```
$ myapp input.txt
$ myapp -v --output=result.txt -n 5 input.txt
$ myapp --help
```
