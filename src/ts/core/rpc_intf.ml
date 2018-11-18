(* Provides RPC interface via WebSocket *)
module Rpc = Jsonrpc_ocaml.Rpc
module R = Jsonrpc_ocaml_jsoo

module type Api_def = Jsonrpc_ocaml_jsoo.Client.Api_def

module type S =
  R.Rpc.S
  with type json = < > Js.t
  with module Thread = Lwt
   and module Response = R.Response
   and module Request = R.Request
