(* An example with flags, options and positional arguments. *)
structure SimpleEntrypoint = struct
  infix |>
  fun x |> f = f x

  (* Define the config type that will hold parsed arguments *)
  type config = { verbose: bool
                , output: string
                , count: int
                , input: string }

  val parser : config Cli.parser =
    Cli.app "simple" "1.0.0" "A simple example CLI application"
      (fn verbose => fn output => fn count => fn input =>
        { verbose = verbose
        , output=output
        , count=count
        , input=input })
    |> Cli.flag { long="verbose"
                , short=SOME #"v"
                , help="Enable verbose output" }
    |> Cli.option { long="output"
                  , short=SOME #"o"
                  , help="Output file path"
                  , placeholder="FILE"
                  , default="output.txt" }
    |> Cli.optionInt { long="count"
                    , short=SOME #"n"
                    , help="Number of iterations"
                    , placeholder="NUM"
                    , default=1 }
    |> Cli.positional { name="input"
                      , help="Input file to process" }

  (* Main function that uses the parsed config *)
  fun handler ({verbose, output, count, input}: config) =
    let
      val _ = if verbose then
                print ("Verbose mode enabled\n" ^
                      "Input: " ^ input ^ "\n" ^
                      "Output: " ^ output ^ "\n" ^
                      "Count: " ^ Int.toString count ^ "\n")
              else ()
      val _ = print ("Processing " ^ input ^ " -> " ^ output ^
                    " (" ^ Int.toString count ^ " times)\n")
    in
      ()
    end

  fun run () = Cli.exec handler (Cli.cli parser)
end



