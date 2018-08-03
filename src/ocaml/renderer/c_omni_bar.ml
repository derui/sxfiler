(** {!C_command_pallet} defines container component to manage command of user. *)
open Sxfiler_core
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

let t =
  let render props =
    let module Dl = C.Locator_intf.Dynamic_registry in
    let module L = (val props##.locator: Locator.Main) in
    let state = S.Command.Store.get @@ S.App.(State.command @@ Store.get L.store) in
    let open Option.Infix in
    let component = state.S.Command.State.current_command >>= fun action ->
      Dl.get L.dynamic_command_registry ~action >>= fun (module Com) ->
      Some [%e div ~class_name:"sf-OmniBar" [Com.(Command.name this) [@txt]]]
    in

    Option.get component ~default:(fun () ->
        [%e div ~class_name:"sf-OmniBar-Command" [command_completer props##.locator]] )
  in
  Component.make render
