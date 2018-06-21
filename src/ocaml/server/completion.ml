(** Completion module defines functions for RPC of completion. *)

(** prefixes of completion module. *)
let module_prefixes = ["completion"]

(** [setup] function will initialize completion module. *)
let setup param = failwith "not implemented yet"

(** [read] function complete current sources with inputted string from parameter, then
return candidates that are completed with input.
 *)
let read param = failwith "not implemented yet"

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in

  S.expose ~_method:Util.(make_method ~module_prefixes ~name:"setup") ~handler:setup server;
  S.expose ~_method:Util.(make_method ~module_prefixes ~name:"read") ~handler:read server;
