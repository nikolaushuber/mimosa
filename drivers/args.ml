open Cmdliner

let file = Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE")

let output_file =
  let parse s =
    match Sys.is_directory s with
    | true -> failwith (Fmt.str "Error: %s is a directory." s)
    | false | (exception Sys_error _) -> Ok (Some s)
  in
  Arg.(
    value
    & opt (conv ~docv:"OUTPUT" (parse, Fmt.(option string))) None
    & info [ "o"; "output" ] ~absent:"stdout" ~docv:"OUTPUT"
        ~doc:"Write to file OUTPUT. Overwrite if file exists.")
