open Lwt
open Websocket
open Websocket_cohttp_lwt

let handler
    (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
    (req  : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t) =
  let open Frame in
  let rpc_server = Jsonrpc_server.make () in
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

let start_server host port () =
  let conn_closed (ch,_) =
    Printf.eprintf "[SERV] connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  let%lwt () = Lwt_io.eprintf "[SERV] Listening for HTTP on port %d\n%!" port in
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback:handler ~conn_closed ())

(* main routine. *)
let () =
  let port = 50879 in
  Lwt_main.run (start_server "localhost" port ())
