(* Provides RPC interface via WebSocket *)
module R = Jsonrpc_ocaml_jsoo

module type S =
  Jsonrpc_ocaml.Server.S
  with type json := < > Js.t
  with module Request := R.Request
   and module Response := R.Response
   and module Thread := Lwt
