(** {!P_command_selector} defines presentation component to select command. *)

module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let t = R.Component.make_stateless
    ~props:(module struct
             class type t = object
               method onFocus: (unit -> unit) Js.readonly_prop
               method onChangeCommand: (string -> unit) Js.readonly_prop
             end
           end)

    ~render:(fun props ->
        let label = [%e label ~key:"labelContainer" ~class_name:"sf-CommandSelector_LabelContainer"
            [[%e span ~key:"label" ~class_name:"sf-CommandSelector_Label" ["Command"]]]
        ] in
        let input = [%e input ~key:"input" ~class_name:"sf-CommandSelector_Input" ~default_value:""
            ~on_input:(fun ev ->
                let value = Js.Opt.get ev##.target##.nodeValue (fun () -> Js.string "") in
                props##.onChangeCommand @@ Js.to_string value
              )
            ~on_focus:(fun _ -> props##.onFocus ())
        ] in

        [%e div ~class_name:"sf-CommandSelector" [label;input]]
      )
