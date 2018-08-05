(** Locate application information. This module as is singleton. *)
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

include C.Locator_intf

module type Main = S with type store = Sxfiler_renderer_store.App.Store.t

module type Store_instance = sig
  type t
  val instance: t
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
    (R:Rpc.Rpc)
    (Ctx: C.Context.Instance)
    (Store:Store_instance with type t := S.App.Store.t): Main = struct
  type store = S.App.Store.t

  let rpc = (module R : Rpc.Rpc)
  let context = (module Ctx : C.Context.Instance)
  let store = Store.instance

  let command_registry = C.Locator_intf.Static_registry.make ()
  let dynamic_command_registry = C.Locator_intf.Dynamic_registry.make ()
end
