(** Completion module defines functions for RPC of completion. *)
module SC = Sxfiler_server_core
module Rpc = Jsonrpc_ocaml_yojson
module T = Sxfiler_types
module Ty = Sxfiler_types_yojson

module File_source = SC.Statable.Make(struct
    type t = T.Node.t list

    let empty () = []
  end)

module Setup_file_sync = Procedure_intf.Make(struct
  include T.Rpc.Rpc_completion.Setup_file_sync
  open Ty.Rpc.Rpc_completion.Setup_file_sync

  let params_of_json = `Required params_of_yojson
  let result_to_json = `Void

  let handle param =
    let module R = SC.Root_state in
    File_source.with_lock (fun source ->
        let%lwt state = Global.Root.get () in
        let ws = match R.find_workspace state ~name:param.workspace_name with
          | None -> raise Rpc.Types.(Jsonrpc_error Error_code.Invalid_request)
          | Some ws -> ws
        in
        let nodes = ws.T.Workspace.current.T.Tree_snapshot.nodes in
        File_source.update nodes
      )
end)

(** [Read_file] module provides to complete current sources with inputted string from parameter, then
    return candidates that are completed with input.
*)
module Read_file_sync = Procedure_intf.Make(struct
  include T.Rpc.Rpc_completion.Read_file_sync
  open Ty.Rpc.Rpc_completion.Read_file_sync

  let params_of_json = `Required params_of_yojson
  let result_to_json = `Result result_to_yojson

  let handle param =
    let module Comp =  Sxfiler_server_completion.Completer in
    let%lwt completer = Global.Completion.get () in
    match completer with
    | None -> raise Rpc.Types.(Jsonrpc_error Error_code.Internal_error)
    | Some completer ->
      File_source.with_lock (fun collection ->
          let module S = (struct
            type t = T.Node.t
            let to_string n = n.T.Node.full_path
          end) in
          let candidates = Comp.read completer ~input:param.input ~collection ~stringify:(module S) in

          List.map (fun c -> T.Types.Candidate.{
              start = c.Comp.start;
              length = c.Comp.length;
              value = c.Comp.value;
            }) candidates
          |> Array.of_list
          |> Lwt.return
        )
end)

module Read_directory_sync = Procedure_intf.Make(struct
    include T.Rpc.Rpc_completion.Read_directory_sync
    open Ty.Rpc.Rpc_completion.Read_directory_sync

    let params_of_json = `Required params_of_yojson
    let result_to_json = `Result result_to_yojson

    let handle param = failwith "not implemented yet"
  end)

module Read_history_sync = Procedure_intf.Make(struct
    include T.Rpc.Rpc_completion.Read_history_sync
    open Ty.Rpc.Rpc_completion.Read_history_sync

    let params_of_json = `Required params_of_yojson
    let result_to_json = `Result result_to_yojson

    let handle param = failwith "not implemented yet"
  end)

let read param = failwith "not implemented yet"

let initialize migemo =
  Global.Completion.update (Some (Sxfiler_server_completion.Completer.make ~migemo))

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module R = T.Rpc.Rpc_completion in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    (R.Setup_file_sync.name, Setup_file_sync.handler);
    (R.Read_file_sync.name, Read_file_sync.handler);
    (R.Read_directory_sync.name, Read_directory_sync.handler);
    (R.Read_history_sync.name, Read_history_sync.handler);
  ]
