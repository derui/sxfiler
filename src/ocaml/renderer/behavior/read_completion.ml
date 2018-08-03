(** This behavior makes initialization to application. *)
module T = Sxfiler_types
module C = Sxfiler_renderer_core

type param = string
type t = {
  rpc: (module C.Rpc_intf.Rpc);
  input: param;
}

type config = (module C.Locator_intf.S)

let create locator param =
  let module L = (val locator : C.Locator_intf.S) in
  {rpc = L.rpc; input = param}

let execute t dispatcher =

  let module RI = Sxfiler_rpc in
  C.Rpc.Client.request t.rpc (module C.Api.Completion.Read_sync)
    (Some {RI.Completion.Read_sync.input = t.input})
    (function
      | Error _ -> ()
      | Ok res ->
        let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
        D.(Dispatcher.dispatch this C.Message.(Completion (Read res)))
    )
