(** C_Key_handler defines that container component to handle key strokes on children component. *)
module T = Sxfiler_domain
module R = Jsoo_reactjs
module Be = Sxfiler_renderer_behavior
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateful(struct
    class type t = object
      (** global keymap *)
      method condition: T.Condition.t Js.readonly_prop
      (** class name for container element.  *)
      method className: string option Js.readonly_prop
      (** specialized key map for this element. If this is None, use global key map instead. *)
      method keymap: string T.Key_map.t Js.readonly_prop
      (** callback function. Calling this function when action was found with key stroke. *)
      method onAction: (string -> unit) Js.readonly_prop
    end
  end)(struct
    type t = unit
  end)

(* convert keyboard event to command. *)
let event_to_action ~ev ~keymap ~condition =
  let module KE = R.Event.Keyboard_event in
  match KE.to_event_type ev with
  | KE.Unknown | KE.KeyPress | KE.KeyUp -> None
  | _ -> begin
      let key = C.Util.keyboard_event_to_key ev in
      T.Key_map.find keymap ~condition ~key
    end

(* execute command when bound action found. *)
let key_handler ~props ev =
  match event_to_action ~ev ~keymap:props##.keymap ~condition:props##.condition with
  | None -> ()
  | Some action -> props##.onAction action

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
             ~others:(object%js
               val tabIndex = Js.string "0"
             end)
             ~on_key_down:(key_handler ~props) [children]]
      )
  in
  Component.make spec
