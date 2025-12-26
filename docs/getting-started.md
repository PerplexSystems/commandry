# Getting Started

Define the pipe operator (`|>`) in your structure to chain parser combinators:

```sml
infix |>
fun x |> f = f x
```

> [!TIP]
> Declare your CLI parsers in a dedicated structure to keep the infix declaration local

## Summary

- [Getting Started](#getting-started)
  - [Summary](#summary)
  - [Declaring a basic parser](#declaring-a-basic-parser)
  - [Running the Parser](#running-the-parser)
  - [Defining Subcommands](#defining-subcommands)
  - [Available Combinators](#available-combinators)
    - [Flags](#flags)
    - [Options](#options)
    - [Optional options](#optional-options)
    - [Positional arguments](#positional-arguments)


## Declaring a basic parser

Create a parser using `Cli.app` and chain argument combinators:

```sml
val parser =
  Cli.app "greet" "1.0.0" "A greeting program"
    (fn name => fn loud => (name, loud))
    |> Cli.positional {name = "name", help = "Name to greet"}
    |> Cli.flag {long = "loud", short = SOME #"l", help = "Shout the greeting"}
```

The function passed to `Cli.app` is curried. Each combinator provides one argument in order.

## Running the Parser

Use `Cli.exec` to run the parser and handle help, version, and errors automatically:

```sml
fun greet (name, loud) =
  let
    val msg = "Hello, " ^ name ^ "!"
  in
    print (if loud then String.map Char.toUpper msg else msg)
  end

fun main () = Cli.exec greet (Cli.cli parser)
```

The `Cli.exec` function:
- Reads command line arguments
- Prints help text for `--help` / `-h`
- Prints version for `--version` / `-V`
- Prints errors to stderr and exits with failure code
- Calls your handler with the parsed result

## Defining Subcommands

Declare a type for you subcommands:

```sml
datatype cmd = 
    Add of string list 
  | Status of bool
```

Create a parser for each subcommand:

```sml
val addCmd =
  Cli.app "add" "" "Add files to index" (fn files => Add files)
    |> Cli.positionalList {name = "files", help = "Files to add"}

val statusCmd =
  Cli.app "status" "" "Show working tree status" (fn short => Status short)
    |> Cli.flag {long = "short", short = SOME #"s", help = "Short format"}
```

Create a parser for your CLI. CLI apps with subcommands, convert parsers to `cli` type and compose them:

```sml
val myCli =
  Cli.app "mygit" "1.0.0" "A git clone" (Status false)
    |> Cli.cli
    |> Cli.subcommand {name = "add", help = "Add files", cmd = addCmd}
    |> Cli.subcommand {name = "status", help = "Show status", cmd = statusCmd}
```

Run with `Cli.exec`, or use `Cli.parse` for custom result handling:

```sml
fun handleCmd (Add files) = (* handle add *)
  | handleCmd (Status short) = (* handle status *)

fun main () = Cli.exec handleCmd myCli

(* Or handle results manually *)
fun main () =
  case Cli.parse myCli (CommandLine.arguments ()) of
      Cli.Ok (Cli.Command cmd) => handleCmd cmd
    | Cli.Ok (Cli.Help text) => print text
    | Cli.Ok (Cli.Version text) => print text
    | Cli.Err e => print (Cli.errorToString e)
```

## Available Combinators

### Flags

Returns `boolean`, defaults to `false`:

```sml
Cli.flag { long = "verbose"
         , short = SOME #"v"
         , help = "Enable verbose output"
         }
```

### Options

Returns `string` or `int`, requires a default value:

```sml
(* string *)
Cli.option { long = "output"
           , short = SOME #"o"
           , help = "Output file"
           , placeholder = "FILE"
           , default = "out.txt"
           }

(* int *)
Cli.optionInt { long = "count"
              , short = SOME #"n"
              , help = "Number of times"
              , placeholder = "NUM"
              , default = 1
              }
```

### Optional options

Returns `string option` or `int option`:

```sml
(* string *)
Cli.optionOpt { long = "config"
              , short = SOME #"c"
              , help = "Config file"
              , placeholder = "FILE"
              }

(* int *)
Cli.optionIntOpt { long = "port"
                 , short = SOME #"p"
                 , help = "Port number"
                 , placeholder = "NUM"
                 }
```

### Positional arguments

```sml
(* required *)
Cli.positional { name = "file"
               , help = "Input file"
               }

(* optional *)
Cli.positionalOpt { name = "file"
                  , help = "Optional file"
                  }

(* zero or more *)
Cli.positionalList { name = "files"
                   , help = "Input files"
                   }
```
