(* An example with two positional arguments for adding numbers *)
structure AddEntrypoint = struct
  infix |>
  fun x |> f = f x

  val parser: (string * string) Cli.parser =
    Cli.app "add" "1.0.0" "Add two numbers"
      (fn a => fn b => (a, b))
      |> Cli.positional {name = "a", help = "First number"}
      |> Cli.positional {name = "b", help = "Second number"}

  fun handler (a, b) =
    case (Int.fromString a, Int.fromString b) of
        (SOME x, SOME y) => print (Int.toString (x + y) ^ "\n")
      | _ => Cli.die "error: arguments must be integers"

  fun run () = Cli.exec handler (Cli.cli parser)
end