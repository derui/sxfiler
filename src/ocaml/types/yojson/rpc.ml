open Sxfiler_types.Rpc

module Completion = struct
  module Setup = struct
    include Completion.Setup

    module Js = struct
      type param = {
        source_type: Types.Source_type.t [@key "sourceType"]
      } [@@deriving yojson]
    end

    let param_of_yojson js =
      let open Ppx_deriving_yojson_runtime in
      Js.param_of_yojson js >>= fun js -> Ok {source_type = js.Js.source_type}
  end

  module Read_common_js = struct
    type param = {
      input: string;
    } [@@deriving yojson]

    type result = Types.Candidate.js array [@@deriving yojson]
  end

  module Read_file = struct
    include Completion.Read_file
    module Js = Read_common_js

    let param_of_yojson js =
      let open Ppx_deriving_yojson_runtime in
      Js.param_of_yojson js >>= fun js -> Ok {input = js.Js.input}

    let result_to_json t =
      let module T = Sxfiler_types.Types in
      Js.result_to_yojson @@ Array.map (fun v -> Types.Candidate.{
          start = v.T.Candidate.start;
          length = v.T.Candidate.length;
          value = Node.to_yojson v.T.Candidate.value;
        }) t
  end

  module Read_directory = struct
    include Completion.Read_directory

    module Js = Read_common_js

    let param_of_yojson js =
      let open Ppx_deriving_yojson_runtime in
      Js.param_of_yojson js >>= fun js -> Ok {input = js.Js.input}

    let result_to_json t =
      let module T = Sxfiler_types.Types in
      Js.result_to_yojson @@ Array.map (fun v -> Types.Candidate.{
          start = v.T.Candidate.start;
          length = v.T.Candidate.length;
          value = Directory_tree.to_yojson v.T.Candidate.value;
        }) t
  end

  module Read_history = struct
    include Completion.Read_history

    module Js = Read_common_js

    let param_of_yojson js =
      let open Ppx_deriving_yojson_runtime in
      Js.param_of_yojson js >>= fun js -> Ok {input = js.Js.input}

    let result_to_json t =
      let module T = Sxfiler_types.Types in
      Js.result_to_yojson @@ Array.map (fun v -> Types.Candidate.{
          start = v.T.Candidate.start;
          length = v.T.Candidate.length;
          value = Snapshot_record.to_yojson v.T.Candidate.value;
        }) t
  end
end
