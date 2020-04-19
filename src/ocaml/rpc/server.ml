include Server_intf
open Sxfiler_core
module I = Sxfiler_infrastructure
module G = Sxfiler_generated
module W = Websocket
module Pb = Ocaml_protoc_plugin

module L = (val I.Logger.make [ "rpc_server" ])

module Command_map = Map.Make (struct
  type t = G.Service.Command.t

  let compare v1 v2 =
    let v1' = G.Service.Command.to_int v1 and v2' = G.Service.Command.to_int v2 in
    Int.compare v1' v2'
end)

module Impl = struct
  type t = {
    actor : (module I.Ws_actor.Instance);
    mutable cancel : unit -> unit;
    waiter : unit Lwt.t;
    wakener : unit Lwt.u;
  }

  let handle service_mapping post_event message =
    let open Result.Infix in
    let response =
      let* message = G.Service.Request.from_proto message |> Result.map_error (fun e -> `Pb_error e) in
      let id = message.id in
      let* endpoint =
        Command_map.find_opt message.command service_mapping
        |> Option.to_result ~none:(`Not_found_service (id, message.command))
      in
      L.debug (fun m -> m "Find endpoint for command: %s" & G.Service.Command.show message.command) |> Lwt.ignore_result;
      endpoint message |> Result.ok
    in
    match response with
    | Ok response ->
        let%lwt response', events = response in
        L.debug (fun m -> m "Response: %s" & G.Service.Response.show response');%lwt
        let%lwt () = post_event events in
        response' |> G.Service.Response.to_proto |> Lwt.return_ok
    | Error e     -> Lwt.return_error e

  let to_frame writer = W.Frame.create ~opcode:W.Frame.Opcode.Binary ~content:(Pb.Writer.contents writer) ()

  let receiver sender service_mapping post_event frame =
    match frame.W.Frame.opcode with
    | W.Frame.Opcode.Binary ->
        let protobuf = frame.W.Frame.content |> Pb.Reader.create in
        let response =
          (* handle and return response *)
          match%lwt handle service_mapping post_event protobuf with
          | Ok r ->
              to_frame r |> sender;
              Lwt.return `Continue
          | Error (`Pb_error _) ->
              L.warn (fun m -> m "Error detected: Can not parse protobuf");%lwt
              Lwt.return `Continue
          | Error (`Not_found_service (id, command)) ->
              let response =
                {
                  G.Service.Response.id;
                  status = G.Service.Status.INVALID_REQUEST_PAYLOAD;
                  payload = Bytes.empty;
                  error =
                    Some
                      {
                        G.Service.Error.status = -2;
                        error_message = Printf.sprintf "Can not handle command: %s" & G.Service.Command.show command;
                        details = [];
                      };
                }
                |> G.Service.Response.to_proto
              in
              let response = to_frame response in
              sender response;
              Lwt.return `Continue
        in

        response
    | _                     -> Lwt.return `Continue

  let start t ~mappings ~post_event =
    let module Actor = (val t.actor) in
    let message_mapping =
      List.fold_left (fun map (key, command) -> Command_map.add key command map) Command_map.empty mappings
    in
    let sender, cancel = Actor.(Actor.make_sender instance) in
    let receiver' = Actor.(Actor.add_receiver instance & receiver sender message_mapping post_event) in
    Lwt.async (fun () -> Lwt.choose [ t.waiter; receiver' ]);
    t.cancel <- cancel;
    t.waiter

  let stop t =
    t.cancel ();
    Lwt.wakeup t.wakener ()
end

let make (module C : I.Ws_actor.Instance) =
  let waiter, wakener = Lwt.wait () in
  ( module struct
    module Server = Impl

    let instance = { Server.actor = (module C); waiter; wakener; cancel = (fun () -> ()) }
  end : Instance )
