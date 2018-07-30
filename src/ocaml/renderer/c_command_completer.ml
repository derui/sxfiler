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

    R.create_element ~key:"command_completer"
      ~props:(object%js
        val completerId = "command"
        val completion = S.Completion.Store.get @@ S.App.State.completion state
        val locator = props##.locator
      end)
      ~children:[
        R.create_element ~props:(object%js
          val onChangeCommand = (fun _ -> failwith "not implemented yet")
        end)
          P_command_selector.t
      ]
      C_completer_wrapper.t
  )
