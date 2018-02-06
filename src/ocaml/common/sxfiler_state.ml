module M = Sxfiler_message
module T = Sxfiler_types
module FFI = Sxfiler_ffi
module Thread = Lwt

(* All state of this application *)
type t = {
  panes: T.Pane.t array;
  current_pane: T.Pane_location.t;
  waiting: bool;
  terminated: bool;
}

class type js = object
  method panes: T.Pane.js Js.t Js.js_array Js.t Js.readonly_prop
  method currentPane: T.Pane_location.t Js.readonly_prop
  method waiting: bool Js.t Js.readonly_prop
  method terminated: bool Js.t Js.readonly_prop
end

let empty = {
  panes = [||];
  current_pane = T.Pane_location.Left;
  waiting = false;
  terminated = false;
}

let to_js : t -> js Js.t = fun t -> object%js
  val panes = Array.map T.Pane.to_js t.panes |> Js.array
  val currentPane = t.current_pane
  val waiting = Js.bool t.waiting
  val terminated = Js.bool t.terminated
end

let of_js : js Js.t -> t = fun t ->
  let panes = Array.map T.Pane.of_js @@ Js.to_array t##.panes in
  {
    panes;
    current_pane = t##.currentPane;
    waiting = Js.to_bool t##.waiting;
    terminated = Js.to_bool t##.terminated
  }
