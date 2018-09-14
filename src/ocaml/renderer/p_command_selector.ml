(** {!P_command_selector} defines presentation component to select command. *)

open Sxfiler_core
module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let key_of_input = "commandInput"

let t =
  R.Component.make_stateful
    ~props:
      ( module struct
        class type t =
          object
            method focused : bool Js.readonly_prop

            method onFocus : (unit -> unit) Js.readonly_prop

            method onBlur : (unit -> unit) Js.readonly_prop

            method onChangeCommand : (string -> unit) Js.readonly_prop
          end
      end )
    ~spec:
      R.(
        component_spec
          ~constructor:(fun this _ -> this##.nodes := Jstable.create ())
          ~initial_custom:(fun _ _ -> object%js end)
          ~initial_state:(fun _ _ ->
              object%js
                val value = Js.string ""
              end )
          ~component_did_update:(fun this props _ ->
              let open Option in
              if props##.focused then
                ignore (R.Ref_table.find ~key:key_of_input this##.nodes >|= fun e -> e##focus)
              else ignore (R.Ref_table.find ~key:key_of_input this##.nodes >|= fun e -> e##blur) )
          (fun this ->
             let label =
               [%e
                 label ~key:"labelContainer" ~class_name:"sf-CommandSelector_LabelContainer"
                   [[%e span ~key:"label" ~class_name:"sf-CommandSelector_Label" ["Command"]]]]
             in
             let input =
               [%e
                 input ~key:"input" ~class_name:"sf-CommandSelector_Input"
                   ~_ref:(fun e -> R.Ref_table.add this##.nodes ~key:key_of_input ~value:e)
                   ~others:
                     (object%js
                       val type_ = Js.string "text"

                       val value = this##.state##.value
                     end)
                   ~on_change:(fun ev ->
                       let value = ev##.target##.value in
                       this##setState
                         (object%js
                           val value = value
                         end) ;
                       this##.props##.onChangeCommand @@ Js.to_string value )
                   ~on_focus:(fun _ -> this##.props##.onFocus ())
                   ~on_blur:(fun _ -> this##.props##.onBlur ())]
             in
             [%e div ~class_name:"sf-CommandSelector" [label; input]] ))
