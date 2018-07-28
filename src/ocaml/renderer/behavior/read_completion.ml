(** This behavior makes initialization to application. *)
module T = Sxfiler_types
module C = Sxfiler_renderer_core

type t = {
  rpc: (module C.Rpc_intf.Rpc);
}

type param = string
type config = (module C.Locator_intf.S)

let make locator =
  let module L = (val locator : C.Locator_intf.S) in
  {rpc = L.rpc}

let execute t dispatcher input =

  let module RI = Sxfiler_rpc in
  `Lwt (C.Rpc.Client.request t.rpc (module C.Api.Completion.Read_sync)
          (Some {RI.Completion.Read_sync.input = input})
          (function
            | Error _ -> ()
            | Ok res ->
              let module D = (val dispatcher : C.Dispatcher_intf.Instance) in
              D.(Dispatcher.dispatch this C.Message.(Completion (Read res)))
          ))
