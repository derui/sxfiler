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

let command_selector =
  R.create_element ~props:(object%js
    val onChangeCommand = (fun _ -> failwith "not implemented yet")
  end)
    P_command_selector.t

let t = Component.make (fun _ ->
    let props = R.element_spec () ~class_name:"fp-CommandPallet" in
    R.Dom.of_tag `div ~props
      ~children:[
      ]
  )
