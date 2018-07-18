open Lwt
open Sxfiler_server
module T = Sxfiler_server_task

exception Fail_load_migemo

let handler
    (rpc_server : Jsonrpc_server.t)
    (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
    (req  : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t) =
  let conn_name = Cohttp.Connection.to_string @@ snd conn in
  let%lwt () = Logs_lwt.info @@ fun m -> m ~tags:(Logger.Tags.module_main ()) "Connection opened: %s" conn_name in
  let uri = Cohttp.Request.uri req in

  match Uri.path uri with
  | "/" ->
    let module C = (val Rpc_connection.make (): Rpc_connection.Instance) in
    let module Handler = Task_result_handler.Make(struct
        let unixtime () = Sxfiler_server_core.Time.time_to_int64 @@ Unix.gettimeofday ()
      end)(Notifier.Impl(C)) in
    let module R = (val T.Runner.make (): T.Runner.Instance) in
    let%lwt () =
      let module I = (val Global.Task_runner.get (): T.Runner.Instance) in
      I.Runner.add_task_handler I.instance ~name:conn_name ~handler:Handler.handle
    in
    let%lwt () = Cohttp_lwt.Body.drain_body body in
    let%lwt (resp, body, frames_out_fn) =
      Websocket_cohttp_lwt.upgrade_connection req (fst conn) (fun f ->
          C.Connection.push_input C.instance ~frame:(Some f)
        )
    in
    (* serve frame/response handler *)
    let%lwt () = C.Connection.connect C.instance frames_out_fn in
    Lwt.ignore_result (
      (* Disable current task when thread is terminated. *)
      let thread = Jsonrpc_server.serve_forever rpc_server (module C) in
      Lwt.on_termination thread (fun () ->
          let module I = (val Global.Task_runner.get (): T.Runner.Instance) in
          Lwt.ignore_result @@ I.Runner.remove_task_handler I.instance ~name:conn_name
        );

      Lwt.join [thread]
    );
    Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
  | _ ->
    Cohttp_lwt_unix.Server.respond_string
      ~status:`Not_found
      ~body:(Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))
      ()

let initialize_modules ~migemo ~keybindings =
  let%lwt () = Completion_op.initialize migemo in
  Global.Keybindings.update keybindings

let start_server _ port ~config:_ =
  let conn_closed (ch,_) =
    Logs.info @@ fun m -> m ~tags:(Logger.Tags.module_main ()) "Connection closed: %s closed"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  let%lwt () = Logs_lwt.info @@ fun m -> m ~tags:(Logger.Tags.module_main ()) "Listening for HTTP on port %d" port in
  let module I = (val Global.Task_runner.get (): T.Runner.Instance) in
  let rpc_server = Jsonrpc_server.make () in

  let rpc_server = Jsonrpc_server.expose rpc_server ~operation:(module Completion_op) in
  let rpc_server = Jsonrpc_server.expose rpc_server ~operation:(module Scanner_op) in
  let rpc_server = Jsonrpc_server.expose rpc_server ~operation:(module Configuration_op) in
  let rpc_server = Jsonrpc_server.expose rpc_server ~operation:(module Keybindings_op) in

  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback:(handler  rpc_server) ~conn_closed ())

(* Load migemo from specified directory that contains dictionary and conversions.  *)
let load_migemo dict_dir =
  let migemo_dict = "migemo-dict"
  and hira_to_kata = "hira2kata.dat"
  and roma_to_hira = "roma2hira.dat"
  and han_to_zen = "han2zen.dat" in

  let dict_file = Filename.concat dict_dir migemo_dict in
  if not @@ Sys.file_exists dict_file then begin
    Logs.err (fun m -> m ~tags:(Logger.Tags.module_main ()) "Dict file not found: %s\n" dict_file);
    raise Fail_load_migemo
  end else
    let module M = Migemocaml in
    match M.Dict_tree.load_dict dict_file with
    | None ->
      Logs.err (fun m -> m ~tags:(Logger.Tags.module_main ()) "Dict can not load: %s" dict_file);
      raise Fail_load_migemo
    | Some migemo_dict ->
      let hira_to_kata =
        Logs.info (fun m -> m ~tags:(Logger.Tags.module_main ()) "Loading %s" hira_to_kata);
        M.Dict_tree.load_conv @@ Filename.concat dict_dir hira_to_kata
      and romaji_to_hira =
        Logs.info (fun m -> m ~tags:(Logger.Tags.module_main ()) "Loading %s" roma_to_hira);
        M.Dict_tree.load_conv @@ Filename.concat dict_dir roma_to_hira
      and han_to_zen =
        Logs.info (fun m -> m ~tags:(Logger.Tags.module_main ()) "Loading %s" han_to_zen);
        M.Dict_tree.load_conv @@ Filename.concat dict_dir han_to_zen
      in
      M.Migemo.make ~dict:migemo_dict ?hira_to_kata ?romaji_to_hira ?han_to_zen ()

(** Load configuration from specified file *)
let load_configuration config =
  let module Y = Sxfiler_types_yojson.Configuration in
  let config = Yojson.Safe.from_file config in
  Some (Y.of_yojson config)

(* Load keymaps from specified file *)
let load_keybindings file = Some (Yojson.Safe.from_file file)

(* Get config from file, but get default when some error happenned  *)
let get_config f config ~default =
  if Sys.file_exists config then
    match f config with
    | None -> default
    | Some config -> config
  else
    default

(* main routine. *)
let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter @@ Logger.lwt_reporter Format.std_formatter;

  let dict_dir = ref "" in
  let config = ref "config.json"
  and key_maps = ref "keymaps.json" in
  let arg_specs = [
    ("-d", Arg.String (fun v -> dict_dir := v), "Directory of migemo dictionary");
    ("--config", Arg.String (fun v -> config := v), "File path for server configuration");
    ("--keymaps", Arg.String (fun v -> key_maps := v), "File path for key maps");
  ] in
  Arg.parse arg_specs ignore "";

  let module C = Sxfiler_types.Configuration in
  let port = 50879 in
  let config = get_config load_configuration !config ~default:C.default in
  let keybindings = get_config load_keybindings !key_maps ~default:`Null in
  let migemo = load_migemo !dict_dir in

  (* setup task runner and finalizer *)
  let module I = (val Global.Task_runner.get (): T.Runner.Instance) in
  let runner_thread = I.Runner.start I.instance ~state:(module Global.Root) in
  Lwt_main.at_exit (fun () ->
      I.Runner.stop I.instance;
      runner_thread
    );

  Lwt_main.run (initialize_modules ~migemo ~keybindings
                >>= fun () -> start_server "localhost" port ~config)
