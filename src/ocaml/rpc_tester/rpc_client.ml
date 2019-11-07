open Websocket
module Base = Jsonrpc_yojson.Client.Make (Lwt)
module S = Sxfiler_server_gateway

module Api = struct
  let filer_make : (S.Filer.Make.Type.input, S.Filer.Make.Type.output) Base.api_def =
    {
      _method = "filer/make";
      params_to_json = S.Filer.Make.Type.input_to_json;
      result_of_json = S.Filer.Make.Type.output_of_json_exn;
    }
end

let get_client send =
  ( module struct
    type json = Base.json

    let call ~api:_ ?params:_ () =
      Lwt.return_error @@ Jsonrpc_yojson.Error.make
      @@ Jsonrpc.Types.Error_code.Application_error ("not implemented", -1)

    let notify ~api ?params () =
      let request = Base.Helper.setup_request ~api ~params in
      let content = Base.Request.to_json request |> Yojson.Safe.to_string in
      send @@ Frame.create ~content ()
  end : Base.S )
