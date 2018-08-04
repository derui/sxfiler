module T = Sxfiler_domain
module P = Sxfiler_server_presenter
module Rpc = Sxfiler_rpc

module Setup_sync = struct
  open Rpc.Completion.Setup_sync

  module Js = struct
    type params = {
      source: P.Completion.Item.t list;
    } [@@deriving yojson]
  end

  let params_to_yojson t =
    Js.params_to_yojson {Js.source = t.source}

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {source = js.Js.source;}
end

module Read_sync = struct
  open Rpc.Completion.Read_sync
  module Js = struct
    type params = {
      input: string;
    } [@@deriving yojson]

    type result = P.Completion.Candidate.t array [@@deriving yojson]
  end

  let params_to_yojson t = Js.params_to_yojson {Js.input = t.input}

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {input = js.Js.input}

  let result_to_yojson t =
    let module T = Sxfiler_domain.Completion in
    Js.result_to_yojson t

  let result_of_yojson js = Js.result_of_yojson js

end
