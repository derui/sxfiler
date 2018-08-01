(** {!P_command_selector} defines presentation component to select command. *)

module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless(struct
    class type t = object
      method onChangeCommand: (string -> unit) Js.readonly_prop
    end
  end)

let t = Component.make (fun props ->
    let label = [%e span ~key:"label" ~class_name:"fp-CommandSelector_Label" ["Command"]] in
    let input = [%e input ~key:"input" ~class_name:"fp-CommandSelector_Input" ~default_value:""
        ~on_input:(fun ev ->
            let value = Js.Opt.get ev##.target##.nodeValue (fun () -> Js.string "") in
            props##.onChangeCommand @@ Js.to_string value
          )
    ] in

    [%e div ~class_name:"fp-CommandSelector" [label;input]]
  )
