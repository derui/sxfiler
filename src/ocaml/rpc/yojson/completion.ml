module T = Sxfiler_types
module Ty = Sxfiler_types_yojson
module Rpc = Sxfiler_rpc

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

  type result = Ty.Types.Candidate.js array [@@deriving yojson]
end

module Read_file_sync = struct
  open Rpc.Completion.Read_file_sync
  module Js = Read_common_js

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {input = js.Js.input}

  let result_to_yojson t =
    let module T = Sxfiler_types.Types in
    Js.result_to_yojson @@ Array.map (fun v -> {
        Ty.Types.Candidate.start = v.T.Candidate.start;
        length = v.T.Candidate.length;
        value = Ty.Node.to_yojson v.T.Candidate.value;
      }) t
end

module Read_directory_sync = struct
  open Rpc.Completion.Read_directory_sync

  module Js = Read_common_js

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {input = js.Js.input}

  let result_to_yojson t =
    let module T = Sxfiler_types.Types in
    Js.result_to_yojson @@ Array.map (fun v -> {
        Ty.Types.Candidate.start = v.T.Candidate.start;
        length = v.T.Candidate.length;
        value = Ty.Directory_tree.to_yojson v.T.Candidate.value;
      }) t
end

module Read_history_sync = struct
  open Rpc.Completion.Read_history_sync

  module Js = Read_common_js

  let params_of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.params_of_yojson js >>= fun js -> Ok {input = js.Js.input}

  let result_to_yojson t =
    let module T = Sxfiler_types.Types in
    Js.result_to_yojson @@ Array.map (fun v -> {
        Ty.Types.Candidate.start = v.T.Candidate.start;
        length = v.T.Candidate.length;
        value = Ty.Snapshot_record.to_yojson v.T.Candidate.value;
      }) t
end
