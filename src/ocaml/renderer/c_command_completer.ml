(** {!C_command_completer} defines container component to input and completer for command. *)

module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless(struct
    class type t = object
      method locator: (module Locator.Main) Js.readonly_prop
    end
  end)

let t = Component.make (fun props ->
    let module L = (val props##.locator) in
    let state = S.App.Store.get L.store in

    [%c C_completer_wrapper.t
        ~completerId:"command"
        ~completion:(S.Completion.Store.get @@ S.App.State.completion state)
        ~locator:props##.locator
          [[%c P_command_selector.t ~key:"completer"
              ~onChangeCommand:(fun _ -> failwith "not implemented yet")]
          ]
    ]
  )
