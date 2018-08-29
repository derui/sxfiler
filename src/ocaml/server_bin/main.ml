open Lwt
open Sxfiler_server
module T = Sxfiler_server_task
module I = Sxfiler_server_infra
module U = Sxfiler_usecase
module G = Sxfiler_server_gateway

exception Fail_load_migemo

let handler (rpc_server : Jsonrpc_server.t) (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
    (req : Cohttp_lwt_unix.Request.t) (body : Cohttp_lwt.Body.t) =
  let conn_name = Cohttp.Connection.to_string @@ snd conn in
  let%lwt () =
    Logs_lwt.info
    @@ fun m -> m ~tags:(Logger.Tags.module_main ()) "Connection opened: %s" conn_name
  in
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/" ->
    let module C = (val Rpc_connection.make () : Rpc_connection.Instance) in
    let module R = (val T.Runner.make () : T.Runner.Instance) in
    let%lwt () = Cohttp_lwt.Body.drain_body body in
    let%lwt resp, body, frames_out_fn =
      Websocket_cohttp_lwt.upgrade_connection req (fst conn) (fun f ->
          C.Connection.push_input C.instance ~frame:(Some f) )
    in
    (* serve frame/response handler *)
    let%lwt () = C.Connection.connect C.instance frames_out_fn in
    Lwt.ignore_result
      ((* Disable current task when thread is terminated. *)
        let thread = Jsonrpc_server.serve_forever rpc_server (module C) in
        Lwt.on_termination thread (fun () -> Logs.info (fun m -> m "Terminate thread")) ;
        Lwt.join [thread]) ;
    Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
  | _ ->
    Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
      ~body:(Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))
      ()

let initialize_modules ~migemo ~keymap ~config =
  let () = Proc_completion.initialize migemo in
  let%lwt () =
    match keymap () with
    | None ->
      Logs.warn (fun m -> m "Detect errors when load keymap. Use default keymap.") ;
      Lwt.return_unit
    | Some keymap ->
      let module R = I.Key_map_repo.Make (Global.Keymap) in
      let module Usecase = U.Keymap.Store (R) in
      let module Gateway = G.Keymap.Store (Usecase) in
      Gateway.handle keymap
  in
  match config () with
  | None ->
    Logs.warn (fun m -> m "Detect errors when load configuration. Use default configuration.") ;
    Lwt.return_unit
  | Some config ->
    let module R = I.Configuration_repo.Make (Global.Root) in
    let module Usecase = U.Configuration.Store (R) in
    let module Gateway = G.Configuration.Store (Usecase) in
    Gateway.handle config

let start_server _ port =
  let conn_closed (ch, _) =
    Logs.info
    @@ fun m ->
    m ~tags:(Logger.Tags.module_main ()) "Connection closed: %s closed"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  let%lwt () =
    Logs_lwt.info
    @@ fun m -> m ~tags:(Logger.Tags.module_main ()) "Listening for HTTP on port %d" port
  in
  let module I = (val Global.Task_runner.get () : T.Runner.Instance) in
  let rpc_server = Jsonrpc_server.make () in
  let rpc_server = Jsonrpc_server.expose rpc_server ~operation:(module Proc_completion) in
  let rpc_server = Jsonrpc_server.expose rpc_server ~operation:(module Proc_filer) in
  let rpc_server = Jsonrpc_server.expose rpc_server ~operation:(module Proc_configuration) in
  let rpc_server = Jsonrpc_server.expose rpc_server ~operation:(module Proc_keymap) in
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback:(handler rpc_server) ~conn_closed ())

(* Load migemo from specified directory that contains dictionary and conversions.  *)
let load_migemo dict_dir =
  let migemo_dict = "migemo-dict"
  and hira_to_kata = "hira2kata.dat"
  and roma_to_hira = "roma2hira.dat"
  and han_to_zen = "han2zen.dat" in
  let dict_file = Filename.concat dict_dir migemo_dict in
  if not @@ Sys.file_exists dict_file then (
    Logs.err (fun m -> m ~tags:(Logger.Tags.module_main ()) "Dict file not found: %s\n" dict_file) ;
    raise Fail_load_migemo )
  else
    let module M = Migemocaml in
    match M.Dict_tree.load_dict dict_file with
    | None ->
      Logs.err (fun m -> m ~tags:(Logger.Tags.module_main ()) "Dict can not load: %s" dict_file) ;
      raise Fail_load_migemo
    | Some migemo_dict ->
      let hira_to_kata =
        Logs.info (fun m -> m ~tags:(Logger.Tags.module_main ()) "Loading %s" hira_to_kata) ;
        M.Dict_tree.load_conv @@ Filename.concat dict_dir hira_to_kata
      and romaji_to_hira =
        Logs.info (fun m -> m ~tags:(Logger.Tags.module_main ()) "Loading %s" roma_to_hira) ;
        M.Dict_tree.load_conv @@ Filename.concat dict_dir roma_to_hira
      and han_to_zen =
        Logs.info (fun m -> m ~tags:(Logger.Tags.module_main ()) "Loading %s" han_to_zen) ;
        M.Dict_tree.load_conv @@ Filename.concat dict_dir han_to_zen
      in
      M.Migemo.make ~dict:migemo_dict ?hira_to_kata ?romaji_to_hira ?han_to_zen ()

(** Load configuration from specified file *)
let load_configuration config =
  let module Y = Sxfiler_server_translator.Configuration in
  let config = Yojson.Safe.from_file config in
  match Y.of_yojson config with Error _ -> None | Ok v -> Some v

(* Load keymaps from specified file *)
let load_keymap file =
  let keymap = Yojson.Safe.from_file file in
  let module Y = Sxfiler_server_translator.Key_map in
  match Y.of_yojson keymap with
  | Error err ->
    Logs.warn (fun m -> m "Error occurred: %s" err) ;
    None
  | Ok v -> Some v

(* Get config from file, but get default when some error happenned  *)
let get_config f config () = if Sys.file_exists config then f config else None

(* main routine. *)
let () =
  Logs.set_level (Some Logs.Info) ;
  Logs.set_reporter @@ Logger.lwt_reporter Format.std_formatter ;
  let dict_dir = ref "" in
  let config = ref "config.json" and key_maps = ref "keymap.json" in
  let arg_specs =
    [ ("-d", Arg.String (fun v -> dict_dir := v), "Directory of migemo dictionary")
    ; ("--config", Arg.String (fun v -> config := v), "File path for server configuration")
    ; ("--keymap", Arg.String (fun v -> key_maps := v), "File path for key maps") ]
  in
  Arg.parse arg_specs ignore "" ;
  let module D = Sxfiler_domain in
  let port = 50879 in
  let config = get_config load_configuration !config in
  let keymap = get_config load_keymap !key_maps in
  let migemo = load_migemo !dict_dir in
  (* setup task runner and finalizer *)
  let module I = (val Global.Task_runner.get () : T.Runner.Instance) in
  let runner_thread = I.Runner.start I.instance in
  Lwt_main.at_exit (fun () -> I.Runner.stop I.instance ; runner_thread) ;
  Lwt_main.run
    (initialize_modules ~migemo ~keymap ~config >>= fun () -> start_server "localhost" port)
