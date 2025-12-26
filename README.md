# commandry

**commandry** is a command line parser for Standard ML.

```sml
val cli: string Cli.parser =
  Cli.app "hello" "1.0.0" "Says hello to someone"
    (fn name => name)
    |> Cli.positional {name = "name", help = "Someone's name"}
    |> Cli.cli

fun main () =
  Cli.exec (fn name => print ("Hello, " ^ name)) cli
```

## Features

- **Declarative builder pattern**: Construct command line by chaining functions together
- **Rich argument types**: Supports flags, string/int options with defaults, and positional arguments (required, optional or variadic)
- **Nested subcommands**: Create hierarchical command structures
- **Auto-generated help**: Automatic `--help` and `--version` flags with formatted usage text

## Installation

The library is a single file, copy [`src/cli.sml`](./src/cli.sml) into your project.

## Documentation

See the [`docs/`](./docs/) directory for detailed guides:

- [Getting Started](./docs/getting-started.md)
- [API Reference](./docs/api-reference.md)
- [Examples](./docs/examples/README.md)