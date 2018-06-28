(** Completion module defines functions for RPC of completion. *)
module Rpc = Jsonrpc_ocaml_yojson

(** prefixes of completion module. *)
let module_prefixes = ["completion"]

let mutex = Lwt_mutex.create ()
let state = ref None

module Setup = struct
  module T = Sxfiler_types_yojson
  type param = {
    source_type: T.Types.Source_type.t [@key "sourceType"];
  } [@@deriving yojson]

  let handler request =
    let module Req = Jsonrpc_ocaml_yojson.Request in
    match request.Req.params with
    | None -> raise Rpc.Types.(Jsonrpc_error Error_code.Invalid_params)
    | Some params -> begin match param_of_yojson params with
        | Error _ -> raise Jsonrpc_ocaml_yojson.Types.(Jsonrpc_error Error_code.Invalid_params)
        | Ok param -> begin match param.source_type with
            | T.Types.Source_type.File -> failwith "not implemented yet"
            | Directory_tree -> failwith "not implemented yet"
            | History -> failwith "not implemented yet"
          end
      end
end

(** [read] function complete current sources with inputted string from parameter, then
    return candidates that are completed with input.
*)
let read param = failwith "not implemented yet"

let initialize migemo =
  state := Some (Sxfiler_server_completion.Completer.make ~migemo)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:Util.(make_method ~module_prefixes ~name) ~handler server
    ) server [
    ("setup", Setup.handler);
    ("read", read);
  ]
