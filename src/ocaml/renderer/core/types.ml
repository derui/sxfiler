module Ty = Sxfiler_types

(** This module provides storing context indicators. *)
module Viewer_context = struct
  type t = {
      (* the context value indicates current focusing element is file tree.  *)
      in_file_tree: bool;
    }

  (** [empty] returns empty element of [t] *)
  let empty = {
      in_file_tree = false;
    }
end

module Viewer_module = struct
  type t =
    | Default
    | File_tree
    | Third_party of string

  let of_string = function
    | "default" -> Default
    | "file_tree" -> File_tree
    | _ as m -> Third_party m

  let to_string = function
    | Default -> "default"
    | File_tree -> "file_tree"
    | Third_party s -> s
end

module File_tree = struct
  type tree = {
    scanner: Ty.Scanner.t;
    selected_item_index: int;
  }

  type t = {
    scanners: tree Jstable.t;
    scanner_order: string * string;
  }

  let make order =
    {
      scanners = Jstable.create ();
      scanner_order = order;
    }

  let swap_order t = {t with scanner_order = Sxfiler_core.Tuple.swap t.scanner_order}

  let update t ~scanner =
    Jstable.add t.scanners Js.(string scanner.Ty.Scanner.name) {scanner; selected_item_index = 0};
    t

  let to_list t =
    let order1, order2 = t.scanner_order in
    let list = [
      Jstable.find t.scanners Js.(string order1) |> Js.Optdef.to_option;
      Jstable.find t.scanners Js.(string order2) |> Js.Optdef.to_option;
    ] in
    let open Sxfiler_core.Option in
    List.filter is_some list |> List.map get_exn

end
