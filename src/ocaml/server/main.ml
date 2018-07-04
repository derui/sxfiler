open Lwt
open Websocket
open Websocket_cohttp_lwt
module Comp = Completion_op
module W = Workspace_op
module T = Sxfiler_server_task

exception Fail_load_migemo

let handler
    (rpc_server : Jsonrpc_server.t)
    (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
    (req  : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t) =
  let open Frame in
  let%lwt () = Lwt_io.eprintf "[CONN] %s\n%!" (Cohttp.Connection.to_string @@ snd conn) in
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/" ->
    let%lwt () = Cohttp_lwt.Body.drain_body body in
    let%lwt (resp, body, frames_out_fn) =
      Websocket_cohttp_lwt.upgrade_connection req (fst conn) (fun f ->
          rpc_server.Jsonrpc_server.frame_writer @@ Some f
        )
    in
    (* serve frame/response handler *)
    let _ = Jsonrpc_server.serve_forever rpc_server frames_out_fn in
    Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
  | _ ->
    Cohttp_lwt_unix.Server.respond_string
      ~status:`Not_found
      ~body:(Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))
      ()

let initialize_modules ~migemo =
  Comp.initialize migemo

let start_server host port ~config ~keymaps ~migemo =
  let conn_closed (ch,_) =
    Printf.eprintf "[SERV] connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  let%lwt () = Lwt_io.eprintf "[SERV] Listening for HTTP on port %d\n%!" port in
  let rpc_server = Jsonrpc_server.make () in
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
  if not @@ Sys.file_exists dict_file then begin
    Printf.printf "Dict file not found: %s\n" dict_file;
    raise Fail_load_migemo
  end
  else begin
    let module M = Migemocaml in
    match M.Dict_tree.load_dict dict_file with
    | None -> begin
        Printf.printf "Dict can not load: %s\n" dict_file;
        raise Fail_load_migemo
      end
    | Some migemo_dict -> begin
        let hira_to_kata = Printf.printf "Loading %s\n" hira_to_kata; M.Dict_tree.load_conv @@ Filename.concat dict_dir hira_to_kata
        and romaji_to_hira = Printf.printf "Loading %s\n" roma_to_hira; M.Dict_tree.load_conv @@ Filename.concat dict_dir roma_to_hira
        and han_to_zen = Printf.printf "Loading %s\n" han_to_zen; M.Dict_tree.load_conv @@ Filename.concat dict_dir han_to_zen
        in
        M.Migemo.make ~dict:migemo_dict ?hira_to_kata ?romaji_to_hira ?han_to_zen ()
      end
  end

(** Load configuration from specified file *)
let load_configuration config =
  let module Y = Sxfiler_types_yojson.Configuration in
  let config = Yojson.Safe.from_file config in
  Some (Y.of_yojson config)

(* Load keymaps from specified file *)
let load_keymaps file =
  let module Y = Sxfiler_types_yojson.Configuration in
  let config = Yojson.Safe.from_file file in
  Some (Y.Key_maps.of_yojson config)

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
  let keymaps = get_config load_keymaps !key_maps ~default:C.Key_maps.default in
  let migemo = load_migemo !dict_dir in

  initialize_modules migemo;

  (* setup task runner and finalizer *)
  let stopper_wakener, stopper = T.Runner.start State.get_current_state in
  Lwt_main.at_exit (fun () -> Lwt.wakeup stopper_wakener (); stopper);
  Lwt_main.run (start_server "localhost" port ~migemo ~config ~keymaps)
