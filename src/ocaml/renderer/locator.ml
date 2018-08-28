(** Locate application information. This module as is singleton. *)
open Locator_abbrevs

include Locator_intf

module type Store_instance = sig
  val this : S.App.Store.t
end

let make_store () =
  let module C = Sxfiler_renderer_core in
  let config = S.Config.(Store.make @@ State.make ())
  and file_list = S.File_list.(Store.make @@ State.make ())
  and keymap = S.Keymap.(Store.make @@ State.make ())
  and completion = S.Completion.(Store.make @@ State.make ())
  and command = S.Command.(Store.make @@ State.make ())
  and workspace = S.Workspace.(Store.make @@ State.make ()) in
  let state = S.App.State.make ~config ~file_list ~keymap ~completion ~command ~workspace in
  S.App.Store.make state


module Make (Client : C.Rpc.Client) (Ctx : C.Context.Instance) (Store : Store_instance) : S =
struct
  let client = (module Client : C.Rpc.Client)
  let context = (module Ctx : C.Context.Instance)
  let store = Store.this
  let command_registry = Command.Static_registry.make ()
  let dynamic_command_registry = Command.Dynamic_registry.make ()

  let service_registry =
    ( module struct
      module I = Sxfiler_renderer_service_impl

      let configuration () = (module I.Configuration.Make (Client) : Service.Configuration.S)
      let keymap () = (module I.Keymap.Make (Client) : Service.Keymap.S)
      let filer () = (module I.Filer.Make (Client) : Service.Filer.S)
      let completion () = (module I.Completion.Make (Client) : Service.Completion.S)
    end
    : Service.Service_registry.S )
end
