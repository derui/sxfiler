(** This module defines behavior for refresh scanner. *)
module T = Sxfiler_types_jsoo
module C = Sxfiler_renderer_core

type t = {
  rpc: (module C.Rpc_intf.Rpc);
  param: string;
}

type param = string
type config = (module C.Locator_intf.S)

let create locator param =
  let module L = (val locator : C.Locator_intf.S) in
  {rpc = L.rpc; param}

let execute t dispatcher =

  let module RI = Sxfiler_rpc in
  C.Rpc.Client.request t.rpc (module C.Api.Scanner.Get_sync)
    (Some {RI.Scanner.Get_sync.name = t.param})
    (function
      | Error _ -> ()
      | Ok res -> let module DI = (val dispatcher: C.Dispatcher_intf.Instance) in
        DI.(Dispatcher.dispatch this C.Message.(Update_scanner res))
    )
