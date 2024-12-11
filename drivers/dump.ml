open Cmdliner
open Mimosa

let main_dump_ptree files =
  let open Reserr in
  let open Fmt in
  pp false (list Ptree_printer.pp) stdout (map Parse.f files)

let ptree =
  Cmd.(
    v
      (info "ptree" ~doc:"Dump the parsetree.")
      Term.(const main_dump_ptree $ Args.files))

let main_pack_dep files =
  let open Reserr in
  let open Fmt in
  pp false (list string) stdout
    (map Parse.f files
    >>= Dependency.f
    >>= map (fun p -> p.Ptree.ppack_name.txt |> ok))

let packdep =
  Cmd.(
    v
      (info "packdep" ~doc:"Dump package order after dependency resolution.")
      Term.(const main_pack_dep $ Args.files))

let main_eqorder files =
  let open Reserr in
  let open Fmt in
  pp false (list Ptree_printer.pp) stdout
    (map Parse.f files >>= Dependency.f >>= Eq_ordering.f)

let eqorder =
  Cmd.(
    v
      (info "eqorder" ~doc:"Dump parsetree after equation ordering.")
      Term.(const main_eqorder $ Args.files))

let main_dump_ttree files =
  let open Reserr in
  let open Fmt in
  pp false (list Ttree_printer.pp) stdout
    (map Parse.f files
    >>= Dependency.f
    >>= Eq_ordering.f
    >>= Step_ordering.f
    >>= Typecheck.f)

let ttree =
  Cmd.(
    v
      (info "ttree" ~doc:"Dump AST after type checking.")
      Term.(const main_dump_ttree $ Args.files))

let main_mono files =
  let open Reserr in
  let open Fmt in
  pp false (list Ttree_printer.pp) stdout
    (map Parse.f files
    >>= Dependency.f
    >>= Eq_ordering.f
    >>= Step_ordering.f
    >>= Typecheck.f
    >>= Monomorphise.f)

let mono =
  Cmd.(
    v
      (info "mono" ~doc:"Dump AST after monomorphisation.")
      Term.(const main_mono $ Args.files))

let main_norm files =
  let open Reserr in
  let open Fmt in
  (list Norm_printer.pp) stdout
    (map Parse.f files
    >>= Dependency.f
    >>= Eq_ordering.f
    >>= Step_ordering.f
    >>= Typecheck.f
    >>= Monomorphise.f
    |> Reserr.unpack
    |> List.map Normalise.f)

let norm =
  Cmd.(
    v
      (info "norm" ~doc:"Dump AST after normalisation.")
      Term.(const main_norm $ Args.files))

let main_ooir files =
  let open Reserr in
  let open Fmt in
  (list Ooir_printer.pp) stdout
    (map Parse.f files
    >>= Dependency.f
    >>= Eq_ordering.f
    >>= Step_ordering.f
    >>= Typecheck.f
    >>= Monomorphise.f
    |> Reserr.unpack
    |> List.map Normalise.f
    |> List.map Objectify.f)

let ooir =
  Cmd.(
    v (info "ooir" ~doc:"Dump object IR.") Term.(const main_ooir $ Args.files))

let main_c files =
  let open Reserr in
  let open Fmt in
  (list C_printer.pp) stdout
    (map Parse.f files
    >>= Dependency.f
    >>= Eq_ordering.f
    >>= Step_ordering.f
    >>= Typecheck.f
    >>= Monomorphise.f
    |> Reserr.unpack
    |> List.map Normalise.f
    |> List.map Objectify.f
    |> List.map Ccomp.f)

let c = Cmd.(v (info "c" ~doc:"Dump C code.") Term.(const main_c $ Args.files))

let cmd =
  let doc = "Dump tool for debugging information." in
  let info = Cmd.info "dump" ~doc in
  let cmds = [ ptree; packdep; eqorder; ttree; mono; norm; ooir; c ] in
  Cmd.group info cmds
