(** C_Key_handler defines that container component to handle key strokes on children component. *)
open Sxfiler_core
module T = Sxfiler_types
module R = Jsoo_reactjs
module Be = Sxfiler_renderer_behavior
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateful(struct
    class type t = object
      (** global locator *)
      method locator: (module Locator.Main) Js.readonly_prop
      (** class name for container element.  *)
      method className: string option Js.readonly_prop
      (** specialized key map for this element. If this is None, use global key map instead. *)
      method keymap: C.Key_map.t option Js.readonly_prop
    end
  end)(struct
    type t = unit
  end)

(* convert keyboard event to command. *)
let event_to_action ~ev ~keymap ~condition =
  let module K = C.Key_map in
  let module KE = R.Event.Keyboard_event in
  let bindings = C.Key_map.find keymap ~condition in
  match KE.to_event_type ev with
  | KE.Unknown | KE.KeyPress | KE.KeyUp -> None
  | _ -> begin
      let key = C.Util.keyboard_event_to_key ev in
      C.Key_bindings.find bindings ~key
    end

(* execute command when bound action found. *)
let key_handler ~props ev =
  let module L = (val props##.locator : Locator.Main) in
  let module Ctx = (val L.context) in
  let app = S.App.Store.get L.store in
  let keymap = Option.get ~default:(fun () -> S.Keymap.Store.get @@ S.App.State.keymap app) props##.keymap in
  let condition = S.Config.State.condition @@ S.Config.Store.get @@ S.App.State.config app in
  match event_to_action ~ev ~keymap ~condition with
  | None -> ()
  | Some action ->
    let module B = (val C.Behavior.make_instance (module Be.Select_action) ~config:() ~param:action) in
    Lwt.ignore_result @@ Ctx.(Context.execute this (module B))

let container_key = "keyHandlerContainer"
let t =
  let spec = R.component_spec
      (fun this ->
         let props = this##.props in
         let class_name = match props##.className with
           | None -> Classnames.to_string []
           | Some v -> Classnames.to_string [(v, true)]
         in
         let children = R.Children.to_element this##.props_defined##.children in
         [%e div ~class_name
             ~on_key_down:(key_handler ~props) [children]]
      )
  in
  Component.make spec
