module W = Workspace
open Sxfiler_types.Rpc

module Completion = struct
  module Setup_file_sync = struct
    open Completion.Setup_file_sync

    module Js = struct
      type param = {
        workspace_name: string [@key "workspaceName"]
      } [@@deriving yojson]
    end

    let param_of_yojson js =
      let open Ppx_deriving_yojson_runtime in
      Js.param_of_yojson js >>= fun js -> Ok {workspace_name = js.Js.workspace_name}
  end

  module Read_common_js = struct
    type param = {
      input: string;
    } [@@deriving yojson]

    type result = Types.Candidate.js array [@@deriving yojson]
  end

  module Read_file_sync = struct
    open Completion.Read_file_sync
    module Js = Read_common_js

    let param_of_yojson js =
      let open Ppx_deriving_yojson_runtime in
      Js.param_of_yojson js >>= fun js -> Ok {input = js.Js.input}

    let result_to_yojson t =
      let module T = Sxfiler_types.Types in
      Js.result_to_yojson @@ Array.map (fun v -> Types.Candidate.{
          start = v.T.Candidate.start;
          length = v.T.Candidate.length;
          value = Node.to_yojson v.T.Candidate.value;
        }) t
  end

  module Read_directory_sync = struct
    open Completion.Read_directory_sync

    module Js = Read_common_js

    let param_of_yojson js =
      let open Ppx_deriving_yojson_runtime in
      Js.param_of_yojson js >>= fun js -> Ok {input = js.Js.input}

    let result_to_yojson t =
      let module T = Sxfiler_types.Types in
      Js.result_to_yojson @@ Array.map (fun v -> Types.Candidate.{
          start = v.T.Candidate.start;
          length = v.T.Candidate.length;
          value = Directory_tree.to_yojson v.T.Candidate.value;
        }) t
  end

  module Read_history_sync = struct
    open Completion.Read_history_sync

    module Js = Read_common_js

    let param_of_yojson js =
      let open Ppx_deriving_yojson_runtime in
      Js.param_of_yojson js >>= fun js -> Ok {input = js.Js.input}

    let result_to_yojson t =
      let module T = Sxfiler_types.Types in
      Js.result_to_yojson @@ Array.map (fun v -> Types.Candidate.{
          start = v.T.Candidate.start;
          length = v.T.Candidate.length;
          value = Snapshot_record.to_yojson v.T.Candidate.value;
        }) t
  end
end

module Workspace = struct
  module Make_sync = struct
    open Workspace.Make_sync
    module Js = struct
      type param = {
        initial_directory: string [@key "initialDirectory"];
        name: string;
      } [@@deriving yojson]

      type result = {
        created: bool;
      } [@@deriving yojson]
    end

    let param_of_yojson js =
      let open Ppx_deriving_yojson_runtime in
      Js.param_of_yojson js >>= fun js -> Ok {
        initial_directory = js.Js.initial_directory;
        name = js.Js.name;
      }

    let result_to_yojson : result -> Yojson.Safe.json = fun t -> Js.result_to_yojson Js.{
        created = t.created
      }
  end

  module Get_sync = struct
    open Workspace.Get_sync
    module Js = struct
      type param = {
        name: string;
      } [@@deriving yojson]

    end

    let param_of_yojson js =
      let open Ppx_deriving_yojson_runtime in
      Js.param_of_yojson js >>= fun js -> Ok {name = js.Js.name;}

    let result_to_yojson = W.to_yojson
  end
end
