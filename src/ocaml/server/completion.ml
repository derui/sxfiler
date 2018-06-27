(** Completion module defines functions for RPC of completion. *)

(** prefixes of completion module. *)
let module_prefixes = ["completion"]

let mutex = Lwt_mutex.create ()
let state = ref None

(** [setup] function will initialize completion module. *)
let setup param = failwith "not implemented yet"

(** [read] function complete current sources with inputted string from parameter, then
return candidates that are completed with input.
 *)
let read param = failwith "not implemented yet"

let initialize server =
  state := Some (Sxfiler_server_completion.Completer.make ~migemo:server.Jsonrpc_server.migemo)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:Util.(make_method ~module_prefixes ~name) ~handler server
    ) server [
    ("setup", setup);
    ("read", read);
  ]
