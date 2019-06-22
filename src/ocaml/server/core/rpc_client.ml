(** Client to call RPC on the client *)
module W = Websocket

module Jy = Jsonrpc_yojson
module Jyc = Jy.Client.Make (Lwt)
include Jyc

module Make (C : Rpc_connection_intf.Instance) : S = struct
  type json = Yojson.Safe.t

  let call ~api:_ ?params:_ () = Lwt.fail_with "not implement"

  let notify ~api ?params () =
    let req = Jyc.Helper.setup_request ~api ~params in
    let content = Jy.Request.to_json req |> Yojson.Safe.to_string in
    let frame = Some (W.Frame.create ~opcode:W.Frame.Opcode.Text ~content ()) in
    Lwt.return C.(Connection.write_output instance ~frame)
end
