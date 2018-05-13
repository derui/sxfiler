module T = Common_types
module PH = Common_pane_history
module Config = Common_config
module Thread = Lwt

(* All state of this application *)
type t = {
  active_pane: T.Pane.t;
  active_pane_history: PH.t;
  inactive_pane: T.Pane.t;
  inactive_pane_history: PH.t;
}

class type js = object
  method activePane: T.Pane.js Js.t Js.readonly_prop
  method activePaneHistory: PH.js Js.t Js.readonly_prop
  method inactivePane: T.Pane.js Js.t Js.readonly_prop
  method inactivePaneHistory: PH.js Js.t Js.readonly_prop
end

let empty =
  let pane () = T.Pane.make ~directory:"." () in
  {
    active_pane = pane ();
    active_pane_history = PH.empty ();
    inactive_pane = pane ();
    inactive_pane_history = PH.empty ();
  }

let of_js : js Js.t -> t = fun t ->
  {
    active_pane = T.Pane.of_js t##.activePane;
    active_pane_history = PH.of_js t##.activePaneHistory;
    inactive_pane = T.Pane.of_js t##.inactivePane;
    inactive_pane_history = PH.of_js t##.inactivePaneHistory;
  }

(* Utility functions *)

let is_left_active t = t.active_pane.T.Pane.location = `Left
let is_right_active t = t.active_pane.T.Pane.location = `Right

let pane t ~loc =
  match loc with
  | `Left -> if is_left_active t
    then t.active_pane
    else t.inactive_pane
  | `Right -> if is_right_active t
    then t.active_pane
    else t.inactive_pane

let pane_history t ~loc =
  match loc with
  | `Left -> if is_left_active t
    then t.active_pane_history
    else t.inactive_pane_history
  | `Right -> if is_right_active t
    then t.active_pane_history
    else t.inactive_pane_history
