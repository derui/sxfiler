module T = Sxfiler_domain
module P = Sxfiler_server_presenter
module Rpc = Sxfiler_rpc

module Scanner_update = struct
  open Rpc.Notification.Scanner_update

  module Js = struct
    type params = {
      name: string;
      scanner: P.Scanner.t;
    } [@@deriving yojson]
  end

  let params_to_yojson : params -> Yojson.Safe.json = fun t -> Js.params_to_yojson {
      Js.name = t.name;
      scanner = t.scanner;
    }

end
