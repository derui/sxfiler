(** This module defines behavior for refresh scanner. *)
module C = Sxfiler_renderer_core

type t = {
  rpc: (module C.Rpc_intf.Rpc);
}

type param = ()
type config = (module C.Locator_intf.S)

let create locator () =
  let module L = (val locator : C.Locator_intf.S) in
  {rpc = L.rpc}

let execute t dispatcher =

  C.Rpc.Client.request t.rpc (module C.Api.Keymap.Get_sync)
    None
    (function
      | Error _ -> ()
      | Ok keymap -> let module DI = (val dispatcher : C.Dispatcher_intf.Instance) in
        DI.(Dispatcher.dispatch this C.Message.(Update_keymap keymap))
    )
