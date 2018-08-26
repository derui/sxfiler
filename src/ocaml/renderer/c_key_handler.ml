(** C_Key_handler defines that container component to handle key strokes on children component.
    This component can handles bubbled event of onkeydown.
*)
module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core

(* convert keyboard event to command. *)
let event_to_action ~ev ~keymap =
  let module KE = R.Event.Keyboard_event in
  match KE.to_event_type ev with
  | KE.Unknown | KE.KeyPress | KE.KeyUp -> None
  | _ -> begin
      let key = C.Util.keyboard_event_to_key ev in
      match List.find_opt (fun key' -> key'.T.Key_map.key = key) keymap.T.Key_map.bindings with
      | None -> None
      | Some key -> Some key.action
    end

(* execute command when bound action found. *)
let key_handler ~props ev =
  match event_to_action ~ev ~keymap:props##.keymap with
  | None -> ()
  | Some action ->
    ev##preventDefault;
    ev##stopPropagation;
    props##.onAction action

let t = R.Component.make_stateful
    ~props:(module struct
             class type t = object
               (** class name for container element.  *)
               method className: string option Js.readonly_prop
               (** specialized key map for this element. If this is None, use global key map instead. *)
               method keymap: T.Key_map.t Js.readonly_prop
               (** callback function. Calling this function when action was found with key stroke. *)
               method onAction: (string -> unit) Js.readonly_prop
             end
           end)
    ~spec:(
      R.component_spec
        ~initial_state:(fun _ _ -> object%js end)
        ~initial_custom:(fun _ _ -> object%js end)
        (fun this ->
           let props = this##.props in
           let class_name = match props##.className with
             | None -> Classnames.to_string []
             | Some v -> Classnames.to_string [(v, true)]
           in
           let children = R.Children.to_element this##.props_defined##.children in
           [%e div ~class_name ~on_key_down:(key_handler ~props) [children]]
        )
    )
