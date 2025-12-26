(* An example showcasing a git-like CLI with multiple subcommands. *)
structure SubCommandsEntrypoint = struct
  infix |>
  fun x |> f = f x

  (* Result type for all subcommands - must be the same type *)
  datatype cmd =
    Add of { force: bool, files: string list }
  | Commit of { message: string, all: bool }
  | Status of { short: bool }

  (* Build the 'add' subcommand parser *)
  val addCmd: cmd Cli.parser =
    Cli.app "add" "" "Add file contents to the index"
      (fn force => fn files => Add { force = force, files = files }) 
      |> Cli.flag { long = "force"
                  , short = SOME #"f"
                  , help = "Allow adding otherwise ignored files" }
      |> Cli.positionalList { name = "files"
                            , help = "Files to add" }

  (* Build the 'commit' subcommand parser *)
  val commitCmd: cmd Cli.parser =
    Cli.app "commit" "" "Record changes to the repository"
      (fn message => fn all => Commit { message = message, all = all }) 
      |> Cli.option { long = "message"
                    , short = SOME #"m"
                    , help = "Commit message"
                    , placeholder = "MSG"
                    , default = "" }
      |> Cli.flag { long = "all"
                  , short = SOME #"a"
                  , help = "Commit all changed files" }

  (* Build the 'status' subcommand parser *)
  val statusCmd: cmd Cli.parser =
    Cli.app "status" "" "Show the working tree status"
      (fn short => Status { short = short })
      |> Cli.flag { long = "short"
                  , short = SOME #"s"
                  , help = "Give output in short format" }

  (* Build the main CLI with subcommands
  * Note: When a CLI only has subcommands and no flags/options of its own,
  * pass a dummy value to app. This value is never used - subcommand results
  * replace it. *)
  val mainCli: cmd Cli.cli =
    Cli.app "mygit" "0.1.0" "A simple git clone"
      (Status {short = false}) (* dummy - replaced by subcommand result *) 
      |> Cli.cli
      |> Cli.subcommand { name = "add"
                        , help = "Add file contents to index"
                        , cmd = addCmd}
      |> Cli.subcommand { name = "commit"
                        , help = "Record changes to repository"
                        , cmd = commitCmd }
      |> Cli.subcommand { name = "status"
                        , help = "Show working tree status"
                        , cmd = statusCmd }

  fun handler (Add {force, files}) =
        let
          val _ = if force then print "[force] " else ()
          val _ = List.app (fn f => print ("add '" ^ f ^ "'\n")) files
        in
          ()
        end
    | handler (Commit {message, all}) =
        let
          val _ = if all then print "[all] " else ()
          val _ = print ("commit: " ^ message ^ "\n")
        in
          ()
        end
    | handler (Status {short}) =
        let
          val _ =
            if short then print "M  file.txt\n"
            else print "modified:   file.txt\n"
        in
          ()
        end

  fun run () = Cli.exec handler mainCli
end
 