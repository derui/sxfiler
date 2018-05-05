module T = Common_types
module PH = Common_pane_history
module Config = Common_config
module Thread = Lwt

(* All state of this application *)
type t = {
  active_pane: T.Pane_location.t;
  left_pane: T.Pane.t;
  right_pane: T.Pane.t;
  left_pane_history: PH.t;
  right_pane_history: PH.t;
}

class type js = object
  method activePane: Js.js_string Js.t Js.readonly_prop
  method leftPane: T.Pane.js Js.t Js.readonly_prop
  method rightPane: T.Pane.js Js.t Js.readonly_prop
  method leftPaneHistory: PH.js Js.t Js.readonly_prop
  method rightPaneHistory: PH.js Js.t Js.readonly_prop
end

let empty =
  let pane () = T.Pane.make ~directory:"." () in
  {
    active_pane = `Left;
    left_pane = pane ();
    right_pane = pane ();
    left_pane_history = PH.empty ();
    right_pane_history = PH.empty ();
  }

let of_js : js Js.t -> t = fun t ->
  {
    active_pane = T.Pane_location.of_js t##.activePane;
    left_pane = T.Pane.of_js t##.leftPane;
    right_pane = T.Pane.of_js t##.rightPane;
    left_pane_history = PH.of_js t##.leftPaneHistory;
    right_pane_history = PH.of_js t##.rightPaneHistory;
  }

(* Utility functions *)

let is_left_active t = t.active_pane = `Left
let is_right_active t = t.active_pane = `Right
let swap_active_pane t =
  {t with active_pane = match t.active_pane with
       | `Left -> `Right
       | `Right -> `Left
  }

let active_pane_history t =
  if is_left_active t then t.left_pane_history else t.right_pane_history

let pane_history t ~loc =
  match loc with
  | `Left -> t.left_pane_history
  | `Right -> t.right_pane_history

let update_pane_history t ~loc ~history =
  {t with
   left_pane_history = if loc = `Left then history else t.left_pane_history;
   right_pane_history = if loc = `Right then history else t.right_pane_history;
  }

let active_pane t =
  if is_left_active t then t.left_pane else t.right_pane
let inactive_pane t =
  if is_left_active t then t.right_pane else t.left_pane

let update_pane ?loc ~pane t =
  let loc' = match loc with
    | None -> t.active_pane
    | Some loc -> loc
  in
  {t with
   left_pane = if loc' = `Left then pane else t.left_pane;
   right_pane = if loc' = `Right then pane else t.right_pane;
  }
