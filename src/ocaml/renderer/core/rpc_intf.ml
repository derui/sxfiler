(* Provides RPC interface via WebSocket *)
module Rpc = Jsonrpc_ocaml.Rpc_intf
module R = Jsonrpc_ocaml_jsoo

module type Api_def = Jsonrpc_ocaml_jsoo.Client.Api_def
module type Rpc = Rpc.S with module Thread := Lwt
                         and module Response := R.Response
                         and module Request := R.Request

module type Client = sig
  val request :
    (module Api_def with type params = 'a and type result = 'b) ->
    'a option -> (('b option, R.Error.t) result -> unit) -> unit Lwt.t

  val notification :
    (module Api_def with type params = 'a) -> 'a option -> unit Lwt.t
end

module type Server = Jsonrpc_ocaml.Server_intf.S
  with type json := < > Js.t
  with module Request := R.Request
   and module Response := R.Response
   and module Thread := Lwt
