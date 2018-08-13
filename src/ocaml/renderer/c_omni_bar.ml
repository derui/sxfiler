(** {!C_command_pallet} defines container component to manage command of user. *)
open Sxfiler_core
module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let command_completer locator =
  [%c C_command_completer.t ~key:"command_completer" ~props:(object%js val locator = locator end)]

let t = R.Component.make_stateless
    ~props:(module struct
             class type t = object
               method locator: (module Locator.S) Js.readonly_prop
             end
           end)
    ~render:(fun props ->
        let module Dl = C.Command.Dynamic_registry in
        let module L = (val props##.locator: Locator.S) in
        let state = S.Command.Store.get @@ S.App.(State.command @@ Store.get L.store) in
        let open Option.Infix in
        let component = state.S.Command.State.current_command >>= fun name ->
          Dl.get L.dynamic_command_registry ~name >>= fun (module Com) ->
          Some [%e div ~class_name:"sf-OmniBar" [Com.(Command.name this) [@txt]]]
        in

        Option.get component ~default:(fun () ->
            [%e div ~class_name:"sf-OmniBar-Command" [command_completer props##.locator]] )
      )
