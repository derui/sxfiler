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

let command_completer locator =
  [%c C_command_completer.t ~key:"command_completer" ~locator]

let t = Component.make (fun props ->
    let module L = (val props##.locator) in
    [%e div ~class_name:"sf-CommandPallet" [command_completer props##.locator]]
  )
