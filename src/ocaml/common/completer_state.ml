
(** Type of item. Item will only string *)
type item = string

(** Type of cursor position *)
type cursor_pos = int

type t = {
  matched_items: string array;
}

class type js = object
  method matchedItems: Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
end

(** Get empty completer state *)
let empty = {
  matched_items = [||];
}

let of_js : js Js.t -> t = fun js ->
  {
    matched_items = Js.array_map Js.to_string js##.matchedItems |> Js.to_array;
  }
