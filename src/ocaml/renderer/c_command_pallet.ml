(** {!C_command_pallet} defines container component to manage command of user. *)

module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless(struct
    class type t = object
      method locator: (module Locator.Main) Js.readonly_prop
    end
  end)

let command_selector locator state =
  [%c C_completer_wrapper.t
      ~key:"command_completer"
      ~completerId:"command"
      ~completion:(S.Completion.Store.get @@ S.App.State.completion state)
      ~locator

      [
        [%c P_command_selector.t ~key:"completer" ~onChangeCommand:(fun _ -> failwith "")]
      ]
  ]


let t = Component.make (fun props ->
    let module L = (val props##.locator) in
    let state = S.App.Store.get L.store in
    [%e div ~class_name:"sf-CommandPallet" [command_selector props##.locator state]]
  )
