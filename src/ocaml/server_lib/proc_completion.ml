(** Completion module defines functions for RPC of completion. *)
module SC = Sxfiler_server_core
module J = Jsonrpc_ocaml_yojson
module T = Sxfiler_types
module Rpc = Sxfiler_rpc
module Rpcy = Sxfiler_rpc_yojson
module C = Sxfiler_server_completion

module type Common_source = SC.Statable.S with type state = T.Completion.Common_item.t list
module type File_source = SC.Statable.S with type state = T.Node.t list
module type Completer = SC.Statable.S with type state = (module C.Completer.Instance)

module Setup_sync(State:Common_source)
  = Procedure_intf.Make(struct
    include Rpc.Completion.Setup_sync
    open Rpcy.Completion.Setup_sync

    let params_of_json = `Required params_of_yojson
    let result_to_json = `Void

    let handle param =
      State.with_lock (fun _ ->
          State.update param.source
        )
  end)

module Read_sync
    (State:Common_source)
    (Completer:Completer)
  = Procedure_intf.Make(struct
    include Rpc.Completion.Read_sync
    open Rpcy.Completion.Read_sync

    let params_of_json = `Required params_of_yojson
    let result_to_json = `Result result_to_yojson

    let handle param =
      let%lwt completer = Completer.get () in
      let module Comp = (val completer) in
      State.with_lock (fun collection ->
          let candidates = Comp.(
              Completer.read instance ~input:param.input ~collection
                ~stringify:(module struct
                             type t = T.Completion.Common_item.t
                             let to_string t = t.T.Completion.Common_item.value
                           end)
            ) in
          Array.of_list candidates |> Lwt.return
        )

  end)

module Setup_file_sync
    (Root:SC.Statable.S with type state = SC.Root_state.t)
    (State:File_source)
  = Procedure_intf.Make(struct
    include Rpc.Completion.Setup_file_sync
    open Rpcy.Completion.Setup_file_sync

    let params_of_json = `Required params_of_yojson
    let result_to_json = `Void

    let handle param =
      let module R = SC.Root_state in
      State.with_lock (fun _ ->
          let%lwt state = Root.get () in
          let scanner = match R.find_scanner state ~name:param.workspace_name with
            | None -> J.(Exception.raise_error Types.Error_code.Invalid_request)
            | Some ws -> ws
          in
          let nodes = scanner.nodes in
          State.update nodes
        )
  end)

(** [Read_file] module provides to complete current sources with inputted string from parameter, then
    return candidates that are completed with input.
*)
module Read_file_sync
    (State:File_source)
    (Completer:Completer)
  = Procedure_intf.Make(struct
    include Rpc.Completion.Read_file_sync
    open Rpcy.Completion.Read_file_sync

    let params_of_json = `Required params_of_yojson
    let result_to_json = `Result result_to_yojson

    let handle param =
      let%lwt completer = Completer.get () in
      let module Comp = (val completer) in
      State.with_lock (fun collection ->
          let module S = (struct
            type t = T.Node.t
            let to_string n = n.T.Node.full_path
          end) in
          let candidates = Comp.(Completer.read instance ~input:param.input ~collection ~stringify:(module S)) in

          Array.of_list candidates |> Lwt.return
        )
  end)

module Read_history_sync = Procedure_intf.Make(struct
    include Rpc.Completion.Read_history_sync
    open Rpcy.Completion.Read_history_sync

    let params_of_json = `Required params_of_yojson
    let result_to_json = `Result result_to_yojson

    let handle _ = failwith "not implemented yet"
  end)

let read _ = failwith "not implemented yet"

let initialize migemo =
  Global.Completion.update (Sxfiler_server_completion.Completer.make ~migemo)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module C = Rpc.Completion in

  let module Setup_sync = Setup_sync(Global.Common_source) in
  let module Read_sync = Read_sync(Global.Common_source)(Global.Completion) in
  let module Setup_file_sync = Setup_file_sync(Global.Root)(Global.File_source) in
  let module Read_file_sync = Read_file_sync(Global.File_source)(Global.Completion) in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    (C.Setup_sync.name, Setup_sync.handler);
    (C.Read_sync.name, Read_sync.handler);
    (C.Setup_file_sync.name, Setup_file_sync.handler);
    (C.Read_file_sync.name, Read_file_sync.handler);
    (C.Read_history_sync.name, Read_history_sync.handler);
  ]
