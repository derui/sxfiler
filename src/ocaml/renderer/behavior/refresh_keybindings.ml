(** This module defines behavior for refresh scanner. *)
module T = Sxfiler_types_jsoo
module C = Sxfiler_renderer_core

type t = {
  rpc: (module C.Rpc_intf.Rpc);
  repository: (module C.Repository_intf.Keybindings_instance)
}

type param = ()
type result = unit Lwt.t

let make locator =
  let module L = (val locator : C.Locator_intf.S) in
  {rpc = (module L.Rpc);
   repository = (module L.Repository.Keybindings)
  }

let execute t () =

  let module RI = Sxfiler_rpc_jsoo in
  C.Rpc.Client.request t.rpc (module C.Api.Keybindings.Get_sync)
    None
    (function
      | Error _ -> ()
      | Ok res -> let module Repo = (val t.repository : C.Repository_intf.Keybindings_instance) in
        let open Repo in
        let keymap = C.Key_map.of_json res in
        Repo.store instance keymap
    )
