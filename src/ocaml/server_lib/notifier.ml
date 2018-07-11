(** Notifier defines API caller to be used to send notification to peer. *)
module W = Websocket_cohttp_lwt
module J = Jsonrpc_ocaml
module Jy = Jsonrpc_ocaml_yojson

(** Module type to define notifier to send notification to other. *)
module type S = sig

  (** [notify api_def params] notify notification defined by [api_def]. *)
  val notify: (module Jy.Client.Api_def with type params = 'b
                                         and type result = 'c)
    -> 'b option -> unit Lwt.t
end

module Impl(Conn:Rpc_connection.Instance) = struct
  module Rpc : J.Rpc_intf.S with module Thread := Lwt
                             and module Request := Jy.Request
                             and module Response := Jy.Response = struct

    let req_to_frame req =
      let json = Jy.Request.to_json req in
      let content = Yojson.Safe.to_string json in
      Some (W.Frame.create ~opcode:W.Frame.Opcode.Text ~content ())

    let call_api ?handler:_ request =
      let module C = Conn.Connection in
      Lwt.return @@ C.write_output Conn.instance ~frame:(req_to_frame request)
  end

  let notify api_def param =
    let tags = Logger.Tags.module_lib ["notifier"] in
    let req, _ = Jy.Client.make_notification api_def param in
    let%lwt () = Logs_lwt.info @@ fun m -> m ~tags "Send notification to client: {%s}" req.Jy.Request._method in
    Rpc.call_api req
end
