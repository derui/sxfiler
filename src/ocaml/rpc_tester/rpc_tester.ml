open Lwt.Infix
open Websocket
open Websocket_lwt_unix
module S = Sxfiler_server_gateway

let section = Lwt_log.Section.make "rpc tester"

let validate_command command args =
  let module A = Rpc_client.Api in
  match command with
  | _ as v when v = A.filer_make._method ->
      Ok
        ( A.filer_make,
          S.Filer.Make.Type.{ initial_location = List.nth args 0; name = List.nth args 1 } )
  | _ -> Error "Not implemented"

let rpc_shell ~client ~content =
  let module Let_syntax = struct
    let bind v ~f = match v with Error _ as e -> e | Ok v -> f v
  end in
  let%bind command, args =
    match String.split_on_char ' ' content with
    | command :: args -> Ok (command, args)
    | _ -> Error "Invalid input"
  in
  let%bind api, params = validate_command command args in
  let module C = (val client : Rpc_client.Base.S) in
  Ok (C.notify ~api ~params ())

let client uri =
  let open Frame in
  let%lwt endp = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
  let%lwt recv, send =
    Conduit_lwt_unix.(
      let%lwt client = endp_to_client ~ctx:default_ctx endp in
      with_connection ~ctx:default_ctx client uri)
  in
  let react fr =
    match fr.opcode with
    | Opcode.Ping -> send @@ Frame.create ~opcode:Opcode.Pong ()
    | Opcode.Close ->
        (* Immediately echo and pass this last message to the user *)
        ( if String.length fr.content >= 2 then
          send @@ Frame.create ~opcode:Opcode.Close ~content:(String.sub fr.content 0 2) ()
        else send @@ Frame.close 1000 )
        >>= fun () -> Lwt.fail Exit
    | Opcode.Pong -> Lwt.return_unit
    | Opcode.Text | Opcode.Binary -> Lwt_io.printf "> %s\n> %!" fr.content
    | _ -> send @@ Frame.close 1002 >>= fun () -> Lwt.fail Exit
  in
  let client = Rpc_client.get_client send in
  let rec react_forever () = recv () >>= react >>= react_forever in
  let rec pushf () =
    Lwt_io.(read_line_opt stdin) >>= function
    | None ->
        Lwt_log.debug ~section "Got EOF. Sending a close frame." >>= fun () ->
        send @@ Frame.close 1000 >>= pushf
    | Some content ->
        ( match rpc_shell ~client ~content with
        | Error e -> Lwt_log.error ~section e
        | Ok _ -> Lwt.return_unit )
        >>= pushf
  in
  pushf () <?> react_forever ()

let apply_loglevel = function
  | 2 -> Lwt_log.(add_rule "*" Info)
  | 3 -> Lwt_log.(add_rule "*" Debug)
  | _ -> ()

let () =
  let uri = ref "" in
  let speclist = Arg.align [ ("-loglevel", Arg.Int apply_loglevel, "1-3 Set loglevel") ] in
  let anon_fun s = uri := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;
  Lwt_main.run (client (Uri.of_string !uri))
