(* An example with a single positional argument. *)
structure HelloEntrypoint = struct
  infix |>
  fun x |> f = f x

  val parser: string Cli.parser =
    Cli.app "hello" "1.0.0" "Says hello to someone"
      (fn name => name)
      |> Cli.positional {name = "name", help = "Someone's name"}

  fun run () =
    Cli.exec
      (fn name => print ("Hello, " ^ name))
      (Cli.cli parser)
end
