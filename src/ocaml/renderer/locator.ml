(** Locate application information. This module as is singleton. *)
open Locator_abbrevs

include Locator_intf

module type Store_instance = sig
  val this: S.App.Store.t
end

let make_store () =
  let module C = Sxfiler_renderer_core in
  let config = S.Config.(Store.make @@ State.make ())
  and scanner = S.Scanner.(Store.make @@ State.make (Const.scanner_1, Const.scanner_2))
  and keymap = S.Keymap.(Store.make @@ State.make ())
  and completion = S.Completion.(Store.make @@ State.make ())
  and command = S.Command.(Store.make @@ State.make ())
  in
  let state = S.App.State.make ~config ~scanner ~keymap ~completion ~command in
  S.App.Store.make state

module Make
    (Client:C.Rpc.Client)
    (Ctx: C.Context.Instance)
    (Store:Store_instance): S = struct
  let client = (module Client : C.Rpc.Client)
  let context = (module Ctx : C.Context.Instance)
  let store = Store.this

  let command_registry = C.Command.Static_registry.make ()
  let dynamic_command_registry = C.Command.Dynamic_registry.make ()
end
