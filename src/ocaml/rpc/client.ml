include Client_intf
open Sxfiler_core
module D = Sxfiler_domain
module I = Sxfiler_infrastructure
module Pb = Ocaml_protoc_plugin
module W = Websocket
module G = Sxfiler_generated

module Log = (val I.Logger.make [ "rpc_client" ])

module Impl (Id : D.Id_generator.S with type id = string) = struct
  type t = { actor : (module I.Ws_actor.Instance) }

  let pb_to_binary_frame writer =
    let content = Pb.Writer.contents writer in
    W.Frame.create ~opcode:W.Frame.Opcode.Binary ~content ()

  let binary_frame_to_pb frame =
    let content = frame.W.Frame.content in
    Pb.Reader.create content

  let call (type r w) t (command : (r, w) Client_command.t) request =
    let payload = command.Client_command.writer request |> Pb.Writer.contents |> Bytes.of_string in
    let module C = (val t.actor) in
    let id = Id.generate () in
    let sender, cancel = C.(Actor.make_sender instance) in
    let response_var = Lwt_mvar.create_empty () in
    let convert_error v = Result.map_error (fun e -> `Pb_error e) v in
    let receiver frame =
      match frame.W.Frame.opcode with
      | W.Frame.Opcode.Binary -> (
          let open Result.Infix in
          let payload =
            let* response = binary_frame_to_pb frame |> G.Service.Response.from_proto |> convert_error in
            if String.equal id response.id then
              response.payload |> Bytes.to_string |> Pb.Reader.create |> command.reader |> convert_error
            else Error `Diff_id
          in
          match payload with
          | Ok payload          ->
              let%lwt () = Lwt_mvar.put response_var (Ok payload) in
              Log.debug (fun m -> m "Receive response for id: %s" id);%lwt
              cancel ();
              Lwt.return `Finished
          | Error `Diff_id      -> Lwt.return `Continue
          | Error (`Pb_error e) ->
              let%lwt () = Lwt_mvar.put response_var (Error (`Pb_error e)) in
              cancel ();
              Lwt.return `Finished)
      | _                     -> Lwt.return `Continue
    in
    let%lwt () = C.(Actor.add_receiver instance receiver) in
    let request =
      { G.Service.Request.id; payload; command = command.command } |> G.Service.Request.to_proto |> pb_to_binary_frame
    in
    sender request;
    Lwt_mvar.take response_var
end

let make (module Id : D.Id_generator.S with type id = string) (module A : I.Ws_actor.Instance) =
  (module struct
    module Client = Impl (Id)

    let instance = { Client.actor = (module A) }
  end : Instance)
