open Parsetree
module Map = Misc.Stdlib.String.Map

let check_global_names name_map pack_item =
  match pack_item.ppack_item with
  | Ppack_step s -> (
      match Map.find_opt s.pstep_name.txt name_map with
      | None -> Map.add s.pstep_name.txt s.pstep_loc name_map |> Reserr.ok
      | Some loc ->
          Error.
            (Non_unique_global_symbol (s.pstep_name.txt, loc), s.pstep_name.loc)
          |> Reserr.error)
  | _ -> failwith "Not yet implemented"

let check { ppack_items; _ } =
  let open Reserr in
  let* _ = fold_left check_global_names Map.empty ppack_items in
  ok ()
