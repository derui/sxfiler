(** Completion module defines functions for RPC of completion. *)
module Rpc = Jsonrpc_ocaml_yojson
module T = Sxfiler_types
module Ty = Sxfiler_types_yojson

(** prefixes of completion module. *)
let module_prefixes = ["completion"]

module Setup_file_sync = Procedure_intf.Make(struct
  include T.Rpc.Completion.Setup_file_sync
  open Ty.Rpc.Completion.Setup_file_sync

  let param_of_json = `Required param_of_yojson
  let result_to_json = `Void

  let handle param = failwith "not implemented yet"
end)

(** [Read_file] module provides to complete current sources with inputted string from parameter, then
    return candidates that are completed with input.
*)
module Read_file_sync = Procedure_intf.Make(struct
  include T.Rpc.Completion.Read_file_sync
  open Ty.Rpc.Completion.Read_file_sync

  let param_of_json = `Required param_of_yojson
  let result_to_json = `Result result_to_yojson

  let handle param = failwith "not implemented yet"
end)

module Read_directory_sync = Procedure_intf.Make(struct
  include T.Rpc.Completion.Read_directory_sync
  open Ty.Rpc.Completion.Read_directory_sync

  let param_of_json = `Required param_of_yojson
  let result_to_json = `Result result_to_yojson

  let handle param = failwith "not implemented yet"
end)

module Read_history_sync = Procedure_intf.Make(struct
  include T.Rpc.Completion.Read_history_sync
  open Ty.Rpc.Completion.Read_history_sync

  let param_of_json = `Required param_of_yojson
  let result_to_json = `Result result_to_yojson

  let handle param = failwith "not implemented yet"
end)

let read param = failwith "not implemented yet"

let initialize migemo =
  Global.Completion.update (Some (Sxfiler_server_completion.Completer.make ~migemo))

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:Util.(make_method ~module_prefixes ~name) ~handler server
    ) server [
    ("setup", Setup_file_sync.handler);
    ("read/file", Read_file_sync.handler);
    ("read/directory", Read_directory_sync.handler);
    ("read/history", Read_history_sync.handler);
  ]
