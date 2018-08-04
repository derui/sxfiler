(** Completion module defines functions for RPC of completion. *)
module SC = Sxfiler_server_core
module T = Sxfiler_domain
module Rpc = Sxfiler_rpc
module C = Sxfiler_server_completion
module G = Sxfiler_server_gateway

module type Cached_source = SC.Statable.S with type state = T.Completion.collection
module type File_source = SC.Statable.S with type state = T.Node.t list
module type Completer = SC.Statable.S with type state = (module C.Completer.Instance)

module Setup_sync(State:Cached_source)
  = Procedure_intf.Make(struct
    include Rpc.Completion.Setup_sync

    let params_of_json = `Required G.Completion.Setup_sync.params_of_yojson
    let result_to_json = `Void

    let handle param =
      State.with_lock (fun _ ->
          State.update param.source
        )
  end)

module Read_sync
    (State:Cached_source)
    (Completer:Completer)
  = Procedure_intf.Make(struct
    include Rpc.Completion.Read_sync

    let params_of_json = `Required G.Completion.Read_sync.params_of_yojson
    let result_to_json = `Result G.Completion.Read_sync.result_to_yojson

    let handle param =
      let%lwt completer = Completer.get () in
      let module Comp = (val completer) in
      State.with_lock (fun collection ->
          let candidates = Comp.(
              Completer.read instance ~input:param.input ~collection
                ~stringify:(module struct
                             type t = T.Completion.Item.t
                             let to_string t = t.T.Completion.Item.value
                           end)
            ) in
          Array.of_list candidates |> Lwt.return
        )

  end)

let initialize migemo =
  Global.Completion.update (Sxfiler_server_completion.Completer.make ~migemo)

let expose server =
  let module S = Jsonrpc_ocaml_yojson.Server in
  let module C = Rpc.Completion in

  let module Setup_sync = Setup_sync(Global.Cached_source) in
  let module Read_sync = Read_sync(Global.Cached_source)(Global.Completion) in

  List.fold_left (fun server (name, handler) ->
      S.expose ~_method:name ~handler server
    ) server [
    (C.Setup_sync.name, Setup_sync.handler);
    (C.Read_sync.name, Read_sync.handler);
  ]
