include Sxfiler_types.Workspace

(* json representation of server uses ppx_deriving_yojson, so this definition
   covers it.
*)

class type js = object
  method current: Tree_snapshot.js Js.t Js.readonly_prop
  method history: Snapshot_history.js Js.t Js.readonly_prop
end

let of_js : js Js.t -> t = fun js ->
  {
    current = Tree_snapshot.of_js js##.current;
    history = Snapshot_history.of_js js##.history;
  }
