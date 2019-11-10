open Lwt
open Sxfiler_server_core
module T = Sxfiler_server_task
module I = Sxfiler_server_infra
module U = Sxfiler_usecase
module D = Sxfiler_domain

exception Fail_load_migemo

module Task_runner = Global.Task_runner (I.Id_generator.Gen_uuid)
module Log = (val Logger.make [ "main" ])

let create_server (module C : Rpc_connection.Instance) (module R : T.Runner.Instance) option =
  let module Completer = (val Global.Completer.get ()) in
  let module Dep =
    Dependencies.Make
      (struct
        let option = option
      end)
      (C)
      (Completer)
      (R)
  in
  let subscribe_task_finished task =
    Dep.Notification_service.send ~typ:I.Task_notification.Finished.typ task.D.Task.id
  in
  let unsubscribe = R.(Runner.subscribe instance ~f:subscribe_task_finished) in
  let rpc_server = Jsonrpc_server.make () in
  (Procedures.expose_all rpc_server (module Dep : Dependencies.S), unsubscribe)

let handler option (conn : _ * Cohttp.Connection.t) (req : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t) =
  let conn_name = Cohttp.Connection.to_string @@ snd conn in
  let%lwt () = Log.info @@ fun m -> m "Connection opened: %s" conn_name in
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/" ->
      let module R = (val Task_runner.get () : T.Runner.Instance) in
      let module C = (val Rpc_connection.make () : Rpc_connection.Instance) in
      let%lwt () = Cohttp_lwt.Body.drain_body body in
      let%lwt resp, frames_out_fn =
        Websocket_cohttp_lwt.upgrade_connection req (fun f ->
            C.Connection.push_input C.instance ~frame:(Some f))
      in
      (* serve frame/response handler *)
      let%lwt () = C.Connection.connect C.instance frames_out_fn in
      Lwt.async (fun () ->
          (* Disable current task when thread is terminated. *)
          let rpc_server, unsubscribe = create_server (module C) (module R) option in
          let thread = Jsonrpc_server.serve_forever rpc_server (module C) in
          Lwt.on_termination thread (fun () ->
              (let%lwt f = unsubscribe in
               let%lwt () = f () in
               Logs.info (fun m -> m "Terminate thread") |> Lwt.return)
              |> Lwt.ignore_result);
          Lwt.join [ thread ]);
      Lwt.return resp
  | _ ->
      let%lwt resp =
        Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
          ~body:(Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))
          ()
      in
      Lwt.return (`Response resp)

(** Load configuration from specified file *)
let load_configuration dir =
  let file = Filename.concat dir "config.json" in
  let module Y = Sxfiler_server_translator.Configuration in
  let config = Yojson.Safe.from_file file in
  match Y.of_json config with Error _ -> None | Ok v -> Some (Y.to_domain v)

(** Load stat file from specified file *)
let load_stat config =
  let module Y = App_state in
  let config = Yojson.Safe.from_file config in
  match Y.of_json config with Error _ -> None | Ok v -> Some v

(* Load keymaps from specified file *)
let load_keymap dir =
  let file = Filename.concat dir "keymap.json" in
  let keymap = Yojson.Safe.from_file file in
  let module Y = Sxfiler_server_translator.Key_map in
  match Y.of_json keymap with
  | Error err ->
      Logs.warn (fun m -> m "Error occurred: %s" @@ Protocol_conv_json.Json.error_to_string_hum err);
      None
  | Ok v -> Some (Y.to_domain v)

(* Get config from file, but get default when some error happenned *)
let get_config f config () = if Sys.file_exists config then f config else None

let initialize_modules ~migemo ~option =
  let completer =
    match migemo with
    | None ->
        Logs.warn (fun m ->
            m "Use fallback completer via forward-match completer because can not load migemo");
        I.Forward_match_completer.make ()
    | Some migemo -> I.Migemo_completer.make ~migemo
  in
  let stat = get_config load_stat option.App_option.stat_file in
  let () = Global.Completer.set @@ fun () -> completer in
  let%lwt () =
    match stat () with
    | None ->
        Logs.info (fun m -> m "Not found application state. Skip restoring");
        Lwt.return_unit
    | Some v ->
        Logs.info (fun m -> m "Restoring persisted stats...");
        Lwt.join
          [
            Global.Root.with_lock (fun state ->
                Logs_lwt.info (fun m -> m "Restoring persisted filers...");%lwt
                let%lwt filers =
                  App_state.restore_filer_stats ~scanner:(module I.Location_scanner_service) v
                in
                let state =
                  List.fold_left (fun state filer -> Root_state.add_filer ~filer state) state filers
                in
                Global.Root.update state;%lwt
                Logs_lwt.info (fun m -> m "Finish restoring persisted filers"));
            Global.Bookmark.with_lock (fun _ ->
                Logs_lwt.info (fun m -> m "Restoring persisted bookmarks...");%lwt
                let bookmarks = App_state.restore_bookmarks v in
                Global.Bookmark.update bookmarks;%lwt
                Logs_lwt.info (fun m -> m "Finish restoring persisted bookmarks"));
          ]
  in
  let config = get_config load_configuration option.App_option.configuration in
  let%lwt () =
    match config () with
    | None ->
        Logs.warn (fun m -> m "Detect errors when load configuration. Use default configuration.");
        Lwt.return_unit
    | Some config -> Global.Configuration.update config
  in
  let keymap = get_config load_keymap option.App_option.configuration in
  match keymap () with
  | None ->
      Logs.warn (fun m -> m "Detect errors when load keymap. Use default keymap.");
      Lwt.return_unit
  | Some keymap -> Global.Keymap.update keymap

let start_server _ option =
  let conn_closed (ch, _) =
    Logs.info @@ fun m ->
    m "Connection closed: %s closed" (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  let%lwt () = Log.info @@ fun m -> m "Listening for HTTP on port %d" option.App_option.port in
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port option.App_option.port))
    (Cohttp_lwt_unix.Server.make_response_action ~callback:(handler option) ~conn_closed ())

(* Load migemo from specified directory that contains dictionary and conversions. *)
let load_migemo dict_dir =
  let migemo_dict = "migemo-dict"
  and hira_to_kata = "hira2kata.dat"
  and roma_to_hira = "roma2hira.dat"
  and han_to_zen = "han2zen.dat" in
  let dict_file = Filename.concat dict_dir migemo_dict in
  if not @@ Sys.file_exists dict_file then (
    Logs.err (fun m -> m "Dict file not found: %s" dict_file);
    None )
  else
    let module M = Migemocaml in
    match M.Dict_tree.load_dict dict_file with
    | None ->
        Logs.err (fun m -> m "Dict can not load: %s" dict_file);
        None
    | Some migemo_dict ->
        let hira_to_kata =
          Logs.info (fun m -> m "Loading %s" hira_to_kata);
          M.Dict_tree.load_conv @@ Filename.concat dict_dir hira_to_kata
        and romaji_to_hira =
          Logs.info (fun m -> m "Loading %s" roma_to_hira);
          M.Dict_tree.load_conv @@ Filename.concat dict_dir roma_to_hira
        and han_to_zen =
          Logs.info (fun m -> m "Loading %s" han_to_zen);
          M.Dict_tree.load_conv @@ Filename.concat dict_dir han_to_zen
        in
        Some (M.Migemo.make ~dict:migemo_dict ?hira_to_kata ?romaji_to_hira ?han_to_zen ())

let persist_app_state global_state ~file_name ~bookmarks =
  Logs.info (fun m -> m "Start app state persisting...");
  let app_state = App_state.empty in
  let filers = Root_state.list_filer global_state in
  let app_state =
    List.fold_left
      (fun state filer ->
        Logs.info (fun m -> m "Persist filer %s..." filer.D.Filer.name);
        App_state.add_filer_stat filer state)
      app_state filers
  in
  Logs.info (fun m -> m "Persist bookmarks...");
  let app_state = App_state.put_bookmarks bookmarks app_state in
  let json = App_state.to_json app_state in
  Yojson.Safe.to_file file_name json;
  Logs.info (fun m -> m "Finish app state persisting")

(* main routine. *)
let () =
  let executable_dir = Filename.dirname Sys.argv.(0) in
  let option = App_option.parse executable_dir in
  Random.init Unix.(gettimeofday () |> int_of_float);
  Logs.set_level (Some (if option.App_option.debug then Logs.Debug else Logs.Info));
  Logs.set_reporter @@ Logger.lwt_reporter Format.std_formatter;
  let module D = Sxfiler_domain in
  let migemo = load_migemo option.migemo_dict_dir in

  (* setup task runner and finalize *)
  let module I = (val Task_runner.get () : T.Runner.Instance) in
  let main_thread =
    initialize_modules ~migemo ~option >>= fun () ->
    start_server "localhost" option <&> I.Runner.start I.instance
  in
  Lwt_main.at_exit (fun () ->
      I.Runner.stop I.instance;
      Log.err (fun m -> m "Exiting in Lwt...");%lwt
      let%lwt state = Global.Root.get () in
      let%lwt bookmarks = Global.Bookmark.get () in
      persist_app_state state ~file_name:option.App_option.stat_file ~bookmarks |> Lwt.return);
  let waiter, wakener = Lwt.task () in
  let rec exit_handler () =
    match%lwt Lwt_io.read_line_opt Lwt_io.stdin with
    | Some "quit" -> Lwt.wakeup wakener () |> Lwt.return
    | Some _ | None -> exit_handler ()
  in
  Lwt_main.run @@ Lwt.choose [ main_thread <&> exit_handler (); waiter ]
