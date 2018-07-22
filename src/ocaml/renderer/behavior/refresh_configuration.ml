(** This module defines behavior for refresh scanner. *)
module T = Sxfiler_types_jsoo
module C = Sxfiler_renderer_core

type message = C.Message.t
type t = {
  rpc: (module C.Rpc_intf.Rpc);
  repository: (module C.Repository_intf.Configuration_instance)
}

type param = ()
type result = unit Lwt.t

let make locator =
  let module L = (val locator : C.Locator_intf.S) in
  {rpc = (module L.Rpc);
   repository = (module L.Repository.Configuration)
  }

let execute t _ () =

  let module RI = Sxfiler_rpc_jsoo in
  C.Rpc.Client.request t.rpc (module C.Api.Configuration.Get_sync)
    None
    (function
      | Error _ -> ()
      | Ok res -> let module Repo = (val t.repository : C.Repository_intf.Configuration_instance) in
        let open Repo in
        Repo.store instance res
    )