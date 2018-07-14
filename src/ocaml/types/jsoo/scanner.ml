(** Scanner module provides type to scan file tree. *)
include Sxfiler_types.Scanner

class type js = object
  method location: Js.js_string Js.t Js.readonly_prop
  method nodes: Node.js Js.t Js.js_array Js.t Js.readonly_prop
  method history: Location_history.js Js.t Js.readonly_prop
end

let of_js : js Js.t -> t = fun js ->
  {
    location = Js.to_string js##.location;
    nodes = Js.array_map Node.of_js js##.nodes |> Js.to_array |> Array.to_list;
    history = Location_history.of_js js##.history;
  }
