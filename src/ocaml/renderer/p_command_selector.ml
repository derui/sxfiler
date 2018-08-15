(** {!P_command_selector} defines presentation component to select command. *)

module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let t = R.Component.make_stateful
    ~props:(module struct
             class type t = object
               method onFocus: (unit -> unit) Js.readonly_prop
               method onChangeCommand: (string -> unit) Js.readonly_prop
             end
           end)
    ~spec:R.(
        component_spec
          ~initial_custom:(fun _ _ -> object%js end)
          ~initial_state:(fun _ _ -> object%js
                           val value = Js.string ""
                         end)
          (fun this ->
             let label = [%e label ~key:"labelContainer" ~class_name:"sf-CommandSelector_LabelContainer"
                 [[%e span ~key:"label" ~class_name:"sf-CommandSelector_Label" ["Command"]]]
             ] in
             let input = [%e input ~key:"input" ~class_name:"sf-CommandSelector_Input"
                 ~others:(object%js
                   val type_ = Js.string "text"
                   val value = this##.state##.value
                 end)
                 ~on_change:(fun ev ->
                     let target = Js.Unsafe.coerce ev##.target in
                     let value = target##.value in
                     this##setState (object%js
                       val value = value
                     end);
                     this##.props##.onChangeCommand @@ Js.to_string value
                   )
                 ~on_focus:(fun _ -> this##.props##.onFocus ())
             ] in

             [%e div ~class_name:"sf-CommandSelector" [label;input]]
          )
      )
