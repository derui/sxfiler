(** This behavior makes initialization to application. *)
module T = Sxfiler_types
module C = Sxfiler_renderer_core

type message = C.Message.t
type t = {
  rpc: (module C.Rpc_intf.Rpc);
}

type param = string
type result = unit Lwt.t

let make locator =
  let module L = (val locator : C.Locator_intf.S) in
  {rpc = (module L.Rpc)}

let execute t dispatcher input =

  let module RI = Sxfiler_rpc in
  C.Rpc.Client.request t.rpc (module C.Api.Completion.Read_sync)
    (Some {RI.Completion.Read_sync.input = input})
    (function
      | Error _ -> ()
      | Ok res ->
        let module D = (val dispatcher : C.Dispatcher_intf.Instance with type message = C.Message.t) in
        D.(Dispatcher.dispatch instance C.Message.(Completion (Read res)))
    )
