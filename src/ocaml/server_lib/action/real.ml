(** This module provides actions in the real world. All function has some of side-effect that is
    to access outer of a program.
*)
open Action_intf
open Sxfiler_core

module No_side_effect : No_side_effect = struct
  let resolve_realpath path =
    let path = Path.of_string (module System.Real) path in
    Path.resolve path |> Path.to_string

  let read_dir ~directory =
    let module T = Sxfiler_types_yojson in
    let items = Sys.readdir directory |> Array.to_list in
    let%lwt nodes = Lwt_list.map_p (fun v -> Lwt.return @@ File_op.get_node directory v) items in
    let%lwt nodes = Lwt.return @@ List.map Option.get_exn @@ List.filter Option.is_some nodes in
    Lwt.return nodes
end

module Side_effect : Side_effect = struct end
