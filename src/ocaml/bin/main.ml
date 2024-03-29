open Sxfiler_core
module I = Sxfiler_infrastructure
module D = Sxfiler_domain
module G = Sxfiler_generated
module F = Sxfiler_workflow
module S = Sxfiler_dependency

exception Fail_load_migemo

module Log = (val I.Logger.make [ "main" ])

(** Load stat file from specified file *)
let load_stat config =
  let module A = App_state in
  Yojson.Basic.from_file config |> A.of_json |> Result.ok

(* Get config from file, but get default when some error happened *)
let get_config f config () = if Sys.file_exists config then f config else Error `Not_exists

let initialize_modules ~migemo ~option =
  let completer =
    match migemo with
    | None        ->
        Logs.warn (fun m -> m "Use fallback completer via forward-match completer because can not load migemo");
        I.Forward_match_completer.make ()
    | Some migemo -> I.Migemo_completer.make ~migemo
  in
  let () = Global.Completer.set & fun () -> completer in
  match Config_loader.load_configuration option () with
  | Error e   ->
      Logs.warn (fun m ->
          m "Detect errors when load configuration. Use default configuration: %s" & Configuration_file.show_error e);
      Lwt.return_unit
  | Ok config -> Global.Configuration_store.update config

(* Load migemo from specified directory that contains dictionary and conversions. *)
let load_migemo dict_dir =
  let migemo_dict = "migemo-dict"
  and hira_to_kata = "hira2kata.dat"
  and roma_to_hira = "roma2hira.dat"
  and han_to_zen = "han2zen.dat" in
  let dict_file = Filename.concat dict_dir migemo_dict in
  if not & Sys.file_exists dict_file then (
    Logs.err (fun m -> m "Dict file not found: %s" dict_file);
    None)
  else
    let module M = Migemocaml in
    match M.Dict_tree.load_dict dict_file with
    | None             ->
        Logs.err (fun m -> m "Dict can not load: %s" dict_file);
        None
    | Some migemo_dict ->
        let hira_to_kata =
          Logs.info (fun m -> m "Loading %s" hira_to_kata);
          M.Dict_tree.load_conv & Filename.concat dict_dir hira_to_kata
        and romaji_to_hira =
          Logs.info (fun m -> m "Loading %s" roma_to_hira);
          M.Dict_tree.load_conv & Filename.concat dict_dir roma_to_hira
        and han_to_zen =
          Logs.info (fun m -> m "Loading %s" han_to_zen);
          M.Dict_tree.load_conv & Filename.concat dict_dir han_to_zen
        in
        Some (M.Migemo.make ~dict:migemo_dict ?hira_to_kata ?romaji_to_hira ?han_to_zen ())

let restore_app_state (module Dep : Dependencies.S) option =
  let stat = get_config load_stat option.App_option.stat_file in
  match stat () with
  | Error _ ->
      Logs.info (fun m -> m "Not found application state. Skip restoring");
      let module F = Sxfiler_workflow in
      let input =
        {
          F.Filer.Initialize.left_location = Path.of_string option.App_option.initial_loc |> Result.get_ok;
          right_location = Path.of_string option.initial_loc |> Result.get_ok;
          left_history = None;
          right_history = None;
          left_sort_order = D.Types.Sort_type.Name;
          right_sort_order = D.Types.Sort_type.Name;
        }
      in
      let open Lwt.Infix in
      let%lwt events =
        F.Filer.initialize input
        |> S.provide (function
             | `Step_file_list_instance c ->
                 S.Context.value (module Dep.Step.File_list : F.Common_step.File_list.Instance) c
             | `Step_filer_instance c     -> S.Context.value (module Dep.Step.Filer : F.Common_step.Filer.Instance) c)
        |> S.run
        >|= List.map (fun v -> F.Filer v)
      in
      let%lwt () = Dep.post_event events in
      Lwt.return_unit
  | Ok v    ->
      Logs.info (fun m -> m "Restoring persisted stats...");
      let restore_filer =
        Log.info (fun m -> m "Restoring persisted filers...");%lwt
        let open Option.Infix in
        let locations =
          let* left_loc, right_loc = App_state_key.(read_by ~key:Key.filer_location v) in
          Some (left_loc |> Path.of_string |> Result.get_ok, right_loc |> Path.of_string |> Result.get_ok)
        in
        let histories =
          let* left_history, right_history = App_state_key.(read_by ~key:Key.filer_histories v) in
          let to_record (timestamp, path) =
            let* location = Path.of_string path |> Result.to_option in
            let* timestamp = Time.of_rfc3339 timestamp in
            Some D.Location_history.Record.(make ~location ~timestamp)
          in
          let to_history records =
            List.fold_left
              (fun histories record ->
                match record with Some record -> D.Location_history.add_record record histories | None -> histories)
              (D.Location_history.make ()) records
          in
          let left_history = left_history |> List.map to_record |> to_history in
          let right_history = right_history |> List.map to_record |> to_history in
          Some (left_history, right_history)
        in
        let initial_locs =
          let initial_loc = Path.of_string option.App_option.initial_loc |> Result.get_ok in
          (initial_loc, initial_loc)
        in
        let left_loc, right_loc = Option.value locations ~default:initial_locs in
        let module F = Sxfiler_workflow in
        let input =
          {
            F.Filer.Initialize.left_location = left_loc;
            right_location = right_loc;
            left_history = Option.map fst histories;
            right_history = Option.map snd histories;
            left_sort_order = D.Types.Sort_type.Name;
            right_sort_order = D.Types.Sort_type.Name;
          }
        in
        let open Lwt.Infix in
        let%lwt events =
          F.Filer.initialize input
          |> S.provide (function
               | `Step_file_list_instance c ->
                   S.Context.value (module Dep.Step.File_list : F.Common_step.File_list.Instance) c
               | `Step_filer_instance c     -> S.Context.value (module Dep.Step.Filer : F.Common_step.Filer.Instance) c)
          |> S.run
          >|= List.map (fun v -> F.Filer v)
        in
        let%lwt () = Dep.post_event events in
        Log.info (fun m -> m "Finish restoring persisted filers")
      in
      let restore_bookmarks =
        Log.info (fun m -> m "Restoring persisted bookmarks...");%lwt
        let%lwt () =
          let bookmarks =
            App_state_key.(read_by ~key:Key.bookmarks v)
            |> Option.value ~default:[]
            |> List.fold_left
                 (fun bookmarks (name, path) ->
                   let name = D.Bookmarks.Name.make name and path = Path.of_string path in
                   match path with
                   | Ok path -> D.Bookmarks.insert ~name ~path bookmarks
                   | Error _ ->
                       Log.info (fun m -> m "Can not restore bookmark: name=%s" (D.Bookmarks.Name.show name))
                       |> Lwt.ignore_result;
                       bookmarks)
                 D.Bookmarks.empty
          in
          Global.Bookmarks.update bookmarks
        in
        Log.info (fun m -> m "Finish restoring persisted bookmarks")
      in
      Lwt.join [ restore_filer; restore_bookmarks ]

let persist_app_state ~file_name =
  Log.info (fun m -> m "Start app state persisting...");%lwt
  let app_state = App_state.create () in

  Log.info (fun m -> m "Persist bookmarks...");%lwt
  (* persist bookmark state *)
  let%lwt bookmarks = Global.Bookmarks.get () in
  let bookmarks =
    D.Bookmarks.items bookmarks
    |> List.map (fun (v : D.Bookmarks.Item.t) -> (v.name |> D.Bookmarks.Name.value, v.path |> Path.to_string))
  in
  App_state_key.(write_by ~key:Key.bookmarks ~value:bookmarks app_state);

  Log.info (fun m -> m "Persist filer status...");%lwt
  let%lwt filer = Global.Filer.get () in
  Option.iter
    (fun (filer : D.Filer.t) ->
      let locations =
        let left = filer.left_file_window.file_list |> D.File_list.location
        and right = filer.right_file_window.file_list |> D.File_list.location in
        (Path.to_string left, Path.to_string right)
      in
      App_state_key.(write_by ~key:Key.filer_location ~value:locations app_state);

      let histories =
        let of_history history =
          history.D.Location_history.records
          |> List.map (fun record ->
                 (record.D.Location_history.Record.timestamp |> Time.to_rfc3339, record.location |> Path.to_string))
        in
        let left = filer.left_file_window.history and right = filer.right_file_window.history in
        (of_history left, of_history right)
      in
      App_state_key.(write_by ~key:Key.filer_histories ~value:histories app_state))
    filer;

  let json = App_state.to_json app_state in
  Yojson.Basic.to_file file_name json;
  Log.info (fun m -> m "Finish app state persisting")

let construct_deps (module Actor : I.Ws_actor.Instance) option =
  let module Completer = (val Global.Completer.get ()) in
  let module Option' = struct
    let option = option
  end in
  let module Dep = (val Dependencies.make (module Option') (module Completer) (module Actor)) in
  (module Dep : Dependencies.S)

let start_rpc_server (module Dep : Dependencies.S) options =
  let mappings = Rpc_service.construct_services (module Dep) options in
  Dep.Server.(Server.start instance ~mappings ~post_event:Dep.post_event)

let call_initial_flows (module Dep : Dependencies.S) option =
  let keymap_file = option.App_option.keymap_file in
  let reload_keymap =
    let%lwt () = Log.info (fun m -> m "Start loading keymap...") in
    let%lwt v =
      F.Keymap.reload { path = keymap_file }
      |> S.provide (function `Step_keymap_instance c ->
             S.Context.value (module Dep.Step.Keymap : F.Common_step.Keymap.Instance) c)
      |> S.run
    in
    Result.fold
      ~ok:(List.map (fun v -> F.Keymap v))
      ~error:(fun e ->
        Log.warn (fun m -> m "Can not load keymap: %s" & F.Keymap.show_error e) |> Lwt.ignore_result;
        [])
      v
    |> Lwt.return
  in
  let%lwt events = Lwt.all [ reload_keymap ] in
  Dep.post_event (List.concat events)

let handler option (conn : _ * Cohttp.Connection.t) (req : Cohttp_lwt_unix.Request.t) (body : Cohttp_lwt.Body.t) =
  let conn_name = Cohttp.Connection.to_string & snd conn in
  let%lwt () = Log.info & fun m -> m "Connection opened: %s" conn_name in
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/" ->
      let module C = (val I.Ws_connection.make () : I.Ws_connection.Instance) in
      let module Ws_actor = (val I.Ws_actor.make (module Dependencies.Id_generator_uuid) (module C)) in
      let%lwt () = Cohttp_lwt.Body.drain_body body in
      let%lwt resp, frames_out_fn =
        Websocket_cohttp_lwt.upgrade_connection req (fun f -> C.Connection.push_input C.instance ~frame:(Some f))
      in
      (* Start actor and server/client as actor *)
      Lwt.async (fun () ->
          (* Disable current task when thread is terminated. *)
          let module Dep = (val construct_deps (module Ws_actor) option) in
          call_initial_flows (module Dep) option;%lwt
          restore_app_state (module Dep) option;%lwt
          let actor_thread = Ws_actor.(Actor.start instance) in
          let server_thread = start_rpc_server (module Dep) option in
          Lwt.on_termination server_thread (fun () ->
              (Dep.Server.(Server.stop instance);
               Ws_actor.(Actor.stop instance);
               Logs.info (fun m -> m "Terminate thread") |> Lwt.return)
              |> Lwt.ignore_result);
          Lwt.join [ server_thread; actor_thread ]);
      let%lwt () = C.Connection.connect C.instance frames_out_fn in
      Lwt.return resp
  | _   ->
      let%lwt resp =
        Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
          ~body:(Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))
          ()
      in
      Lwt.return (`Response resp)

let start_server _ option =
  let conn_closed (ch, _) =
    Logs.info & fun m ->
    m "Connection closed: %s closed" (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  let%lwt () = Log.info & fun m -> m "Listening for HTTP on port %d" option.App_option.port in
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port option.App_option.port))
    (Cohttp_lwt_unix.Server.make_response_action ~callback:(handler option) ~conn_closed ())

(* main routine. *)
let () =
  let executable_dir = Filename.dirname Sys.argv.(0) in
  let option = App_option.parse executable_dir in
  Random.init Unix.(gettimeofday () |> int_of_float);
  Logs.set_level (Some (if option.App_option.debug then Logs.Debug else Logs.Info));
  Logs.set_reporter & I.Logger.lwt_reporter Format.std_formatter;
  let migemo = load_migemo option.migemo_dict_dir in

  (* setup task runner and finalize *)
  let main_thread =
    let open Lwt in
    initialize_modules ~migemo ~option >>= fun () -> start_server "localhost" option
  in
  Lwt_unix.(on_signal Sys.sigint (fun _ -> exit 0)) |> ignore;
  Lwt_main.at_exit (fun () ->
      Log.err (fun m -> m "Exiting in Lwt...");%lwt
      persist_app_state ~file_name:option.App_option.stat_file);
  let waiter, wakener = Lwt.task () in
  let rec exit_handler () =
    match%lwt Lwt_io.read_line_opt Lwt_io.stdin with
    | Some "quit"   -> Lwt.wakeup wakener () |> Lwt.return
    | Some _ | None -> exit_handler ()
  in
  let open Lwt in
  Lwt_main.run & Lwt.choose [ main_thread <&> exit_handler (); waiter ]
