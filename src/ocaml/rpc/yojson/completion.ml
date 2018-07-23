module T = Sxfiler_types
module Ty = Sxfiler_types_yojson
module Rpc = Sxfiler_rpc

module Setup_sync = struct
  open Rpc.Completion.Setup_sync

  module Js = struct
    type params = {
      source: Ty.Completion.Common_item.t list;
    } [@@deriving yojson]
  end

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {source = js.Js.source;}
end

module Setup_file_sync = struct
  open Rpc.Completion.Setup_file_sync

  module Js = struct
    type params = {
      workspace_name: string [@key "workspaceName"]
    } [@@deriving yojson]
  end

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {workspace_name = js.Js.workspace_name}
end

module Read_common_js = struct
  type params = {
    input: string;
  } [@@deriving yojson]

  type result = Ty.Completion.Candidate.js array [@@deriving yojson]
end

module Read_sync = struct
  open Rpc.Completion.Read_sync
  module Js = Read_common_js

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {input = js.Js.input}

  let result_to_yojson t =
    let module T = Sxfiler_types.Completion in
    Js.result_to_yojson @@ Array.map (fun v -> {
          Ty.Completion.Candidate.start = v.T.Candidate.start;
          length = v.T.Candidate.length;
          value = Ty.Completion.Common_item.to_yojson v.T.Candidate.value;
        }) t
end

module Read_file_sync = struct
  open Rpc.Completion.Read_file_sync
  module Js = Read_common_js

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {input = js.Js.input}

  let result_to_yojson t =
    let module T = Sxfiler_types.Completion in
    Js.result_to_yojson @@ Array.map (fun v -> {
          Ty.Completion.Candidate.start = v.T.Candidate.start;
          length = v.T.Candidate.length;
          value = Ty.Node.to_yojson v.T.Candidate.value;
        }) t
end

module Read_history_sync = struct
  open Rpc.Completion.Read_history_sync

  module Js = Read_common_js

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {input = js.Js.input}

  let result_to_yojson t =
    let module T = Sxfiler_types.Completion in
    Js.result_to_yojson @@ Array.map (fun v -> {
          Ty.Completion.Candidate.start = v.T.Candidate.start;
          length = v.T.Candidate.length;
          value = Ty.Location_record.to_yojson v.T.Candidate.value;
        }) t
end
