open Cmdliner
open Mimosa

let fmt_of_path p =
  match p with
  | None -> Fmt.stdout
  | Some p -> open_out p |> Format.formatter_of_out_channel

let main file out =
  let open Reserr in
  let open Fmt in
  let fmt = fmt_of_path out in
  pf fmt "@[<v>%a@]@." Ppxlib_ast.Pprintast.structure
    (Parse.f file
    >>= Ordering.f
    >>= Typecheck.f
    |> Reserr.unpack
    |> Comp_sim.trans_pack)

let cmd =
  Cmd.(
    v
      (info "sim" ~doc:"Compile Mimosa code to OCaml for simulation.")
      Term.(const main $ Args.file $ Args.output_file))
