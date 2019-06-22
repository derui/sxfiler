(** Client to call RPC on the client *)
include module type of Jsonrpc_yojson.Client.Make (Lwt)

module Make (C : Rpc_connection_intf.Instance) : S
