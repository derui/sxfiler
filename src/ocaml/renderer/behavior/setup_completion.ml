(** This behavior makes initialization to application. *)
module T = Sxfiler_types
module C = Sxfiler_renderer_core

type t = {
  rpc: (module C.Rpc_intf.Rpc);
}

type param = T.Completion.Source_class.t * T.Completion.collection * string
type config = (module C.Locator_intf.S)

let make locator =
  let module L = (val locator : C.Locator_intf.S) in
  {rpc = L.rpc}

let execute t dispatcher (cls, source, completer) =

  let module RI = Sxfiler_rpc in
  C.Rpc.Client.request t.rpc (module C.Api.Completion.Setup_sync)
    (Some {RI.Completion.Setup_sync.source = source})
    (function
      | Error _ -> ()
      | Ok _ ->
        let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
        D.(Dispatcher.dispatch this C.Message.(Completion (Setup (cls, completer))))
    )
