(** [Tree_snapshot] has snapshot of a directory in {!Directory_tree}.
    Snapshot has a absolute path of the directory and nodes in the directory.
*)
include Sxfiler_types.Tree_snapshot

class type js = object
  method directory: Js.js_string Js.t Js.readonly_prop
  method nodes: Node.js Js.t Js.js_array Js.t Js.readonly_prop
end

let of_js : js Js.t -> t = fun js ->
  {
    directory = Js.to_string js##.directory;
    nodes = Js.array_map Node.of_js js##.nodes |> Js.to_array |> Array.to_list;
  }
