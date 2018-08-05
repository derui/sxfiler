include Sxfiler_domain.Workspace

(* json representation of server uses ppx_deriving_yojson, so this definition
   covers it.
*)

class type js = object
  method source: Tree_snapshot.js Js.t Js.readonly_prop
  method target: Tree_snapshot.js Js.t Js.readonly_prop
end

let of_js : js Js.t -> t = fun js ->
  {
    source = Tree_snapshot.of_js js##.source;
    target = Tree_snapshot.of_js js##.target;
  }