(* An example with a more complex CLI with multiple optional arguments. *)
structure GitLikeEntrypoint = struct
  infix |>
  fun x |> f = f x

  (* Config type for a hypothetical git-add command *)
  type config = { force: bool
                , dryRun: bool
                , verbose: bool
                , interactive: bool
                , files: string list }

  val parser : config Cli.parser =
    Cli.app "git-add" "1.0.0" "Add file contents to the index"
      (fn force => fn dryRun => fn verbose => fn interactive => fn files =>
        { force = force
        , dryRun = dryRun
        , verbose = verbose
        , interactive = interactive
        , files = files })
    |> Cli.flag { long = "force"
                , short = SOME #"f"
                , help = "Allow adding otherwise ignored files" }
    |> Cli.flag { long = "dry-run"
                , short = SOME #"n"
                , help = "Don't actually add files, just show what would happen" }
    |> Cli.flag { long = "verbose"
                , short = SOME #"v"
                , help = "Be verbose" }
    |> Cli.flag { long = "interactive"
                , short = SOME #"i"
                , help = "Interactive picking" }
    |> Cli.positionalList { name = "files"
                          , help = "Files to add" }

  fun handler ({force, dryRun, verbose, interactive, files}: config) =
    let
      val prefix = if dryRun then "[dry-run] " else ""
      val _ = if verbose then
                print ("Force: " ^ Bool.toString force ^ "\n" ^
                      "Dry run: " ^ Bool.toString dryRun ^ "\n" ^
                      "Interactive: " ^ Bool.toString interactive ^ "\n")
              else ()
      val _ = List.app (fn f =>
                print (prefix ^ "add '" ^ f ^ "'\n")) files
    in
      ()
    end

  fun run () = Cli.exec handler (Cli.cli parser)
end
