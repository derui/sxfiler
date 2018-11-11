(** This module provides interface of RPC client *)

module type S = Jsonrpc_ocaml_jsoo.Client.S with module Thread := Lwt
