(** This module defines behavior for refresh scanner. *)
module T = Sxfiler_types_jsoo
module C = Sxfiler_renderer_core

type t = {
  rpc: (module C.Rpc_intf.Rpc);
  repository: (module C.Repository_intf.Scanner_instance)
}

type param = string
type result = unit Lwt.t

let make locator =
  let module L = (val locator : C.Locator_intf.S) in
  {rpc = (module L.Rpc);
   repository = (module L.Repository.Scanner)
  }

let execute t name =

  let module RI = Sxfiler_rpc in
  C.Rpc.Client.request t.rpc (module C.Api.Scanner.Get_sync)
    (Some {RI.Scanner.Get_sync.name = name})
    (function
      | Error _ -> ()
      | Ok res -> let module Repo = (val t.repository : C.Repository_intf.Scanner_instance) in
        let open Repo in
        Repo.store instance res
    )
