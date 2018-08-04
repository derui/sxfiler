(** This module defines behavior for refresh scanner. *)
module T = Sxfiler_domain_jsoo
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
  C.Rpc.Client.request t.rpc (module C.Api.Keybindings.Get_sync)
    None
    (function
      | Error _ -> ()
      | Ok res -> let module DI = (val dispatcher : C.Dispatcher_intf.Instance) in
        let keymap = T.Key_map.of_js ~conv:(module struct
            type t = string
            let to_json t = Js.Unsafe.coerce @@ Js.string t
            let of_json js = Js.to_string @@ Js.Unsafe.coerce js
          end) res in
        DI.(Dispatcher.dispatch this C.Message.(Update_keymap keymap))
    )
