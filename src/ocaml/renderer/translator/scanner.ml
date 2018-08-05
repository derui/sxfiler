(** Scanner module provides type to scan file tree. *)
open Sxfiler_core
open Sxfiler_domain.Scanner

class type js = object
  method id: Js.js_string Js.t Js.readonly_prop
  method location: Js.js_string Js.t Js.readonly_prop
  method nodes: Node.js Js.t Js.js_array Js.t Js.readonly_prop
  method history: Location_history.js Js.t Js.readonly_prop
end

let of_js ?(system=(module System.Real:System.S)) js : t =
  {
    id = Js.to_string js##.id;
    location = Path.of_string system @@ Js.to_string js##.location;
    nodes = Js.array_map Node.of_js js##.nodes |> Js.to_array |> Array.to_list;
    history = Location_history.of_js js##.history;
  }

let to_js t : js Js.t = object%js
  val id = Js.string t.id
  val location = Js.string @@ Path.to_string t.location
  val nodes = List.map Node.to_js t.nodes |> Array.of_list |> Js.array
  val history = Location_history.to_js t.history
end
