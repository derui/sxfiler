(** Completion module defines functions for RPC of completion. *)
module Rpc = Jsonrpc_ocaml_yojson

(** prefixes of completion module. *)
let module_prefixes = ["completion"]

let mutex = Lwt_mutex.create ()
let state = ref None

module Setup = Procedure_intf.Make(struct
  module T = Sxfiler_types
  module Ty = Sxfiler_types_yojson
  include T.Rpc.Completion.Setup

  let param_of_json = `Required Ty.Rpc.Completion.Setup.param_of_yojson
  let result_to_json = `Void

  let handle param =
    match param.source_type with
    | T.Types.Source_type.File -> failwith "not implemented yet"
    | Directory_tree -> failwith "not implemented yet"
    | History -> failwith "not implemented yet"
end)

(** [Read_file] module provides to complete current sources with inputted string from parameter, then
    return candidates that are completed with input.
*)
module Read_file = Procedure_intf.Make(struct
  module T = Sxfiler_types
  module Ty = Sxfiler_types_yojson
  include T.Rpc.Completion.Read_file

  let param_of_json = `Required Ty.Rpc.Completion.Read_file.param_of_yojson
  let result_to_json = `Result Ty.Rpc.Completion.Read_file.result_to_yojson

  let handle param = failwith "not implemented yet"
end)

module Read_directory = Procedure_intf.Make(struct
  module T = Sxfiler_types
  module Ty = Sxfiler_types_yojson
  include T.Rpc.Completion.Read_directory

  let param_of_json = `Required Ty.Rpc.Completion.Read_directory.param_of_yojson
  let result_to_json = `Result Ty.Rpc.Completion.Read_directory.result_to_yojson

  let handle param = failwith "not implemented yet"
end)

module Read_history = Procedure_intf.Make(struct
  module T = Sxfiler_types
  module Ty = Sxfiler_types_yojson
  include T.Rpc.Completion.Read_history

  let param_of_json = `Required Ty.Rpc.Completion.Read_history.param_of_yojson
  let result_to_json = `Result Ty.Rpc.Completion.Read_history.result_to_yojson

  let handle param = failwith "not implemented yet"
end)

let read param = failwith "not implemented yet"

let initialize migemo =
  state := Some (Sxfiler_server_completion.Completer.make ~migemo)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:Util.(make_method ~module_prefixes ~name) ~handler server
    ) server [
    ("setup", Setup.handler);
    ("read/file", Read_file.handler);
    ("read/directory", Read_directory.handler);
    ("read/history", Read_history.handler);
  ]
