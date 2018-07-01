(** This module provides actions in the real world. All function has some of side-effect that is
    to access outer of a program.
*)
open Action_intf

module No_side_effect : No_side_effect = struct
  let take_snapshot ~directory =
    let module T = Sxfiler_types_yojson in
    let items = Sys.readdir directory |> Array.to_list in
    let open Lwt in
    Lwt_list.map_p (fun v -> Lwt.return @@ File_op.get_node directory v) items
    >|= fun nodes -> T.Tree_snapshot.make ~directory ~nodes
end

module Side_effect : Side_effect = struct end
