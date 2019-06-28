include module type of Jsonrpc_yojson.Client.Make (Lwt)
(** Client to call RPC on the client *)

module Make (C : Rpc_connection_intf.Instance) : S
