(** This module defines behavior for refresh scanner. *)
module T = Sxfiler_types_jsoo
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

  let module RI = Sxfiler_rpc_jsoo in
  let ret = C.Rpc.Client.request t.rpc (module C.Api.Keybindings.Get_sync)
    None
    (function
      | Error _ -> ()
      | Ok res -> let module DI = (val dispatcher : C.Dispatcher_intf.Instance) in
        let keymap = C.Key_map.of_json res in
        DI.(Dispatcher.dispatch this C.Message.(Update_keymap keymap))
    )
  in
  `Lwt ret
