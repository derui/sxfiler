module T = Sxfiler_types
module Tj = Sxfiler_types_jsoo
module Rpc = Sxfiler_rpc

module Setup_file_sync = struct
  open Rpc.Completion.Setup_file_sync

  class type js_param = object
    method workspaceName: Js.js_string Js.t Js.readonly_prop
  end

  let params_to_json : params -> js_param Js.t = fun t -> object%js
    val workspaceName = Js.string t.workspace_name
  end

  let result_of_json _ = ()
end

module Read_common_js = struct
  class type params = object
    method input: Js.js_string Js.t Js.readonly_prop
  end

  type result = Tj.Types.Candidate.js Js.t Js.js_array
end

module Read_file_sync = struct
  open Rpc.Completion.Read_file_sync
  module Jsoo = Read_common_js

  let params_to_json t = object%js
    val input = Js.string t.input
  end

  let result_of_json : Jsoo.result Js.t -> result = fun t ->
    let module T = Sxfiler_types.Types in
    Js.to_array @@ Js.array_map (fun v -> {
          T.Candidate.start = v##.start;
          length = v##.length;
          value = Tj.Node.of_js @@ Js.Unsafe.coerce v##.value;
        }) t
end

module Read_history_sync = struct
  open Rpc.Completion.Read_history_sync
  module Jsoo = Read_common_js

  let params_to_json t = object%js
    val input = Js.string t.input
  end

  let result_of_json : Jsoo.result Js.t -> result = fun t ->
    let module T = Sxfiler_types.Types in
    Js.to_array @@ Js.array_map (fun v -> {
          T.Candidate.start = v##.start;
          length = v##.length;
          value = Tj.Location_record.of_js @@ Js.Unsafe.coerce v##.value;
        }) t
end
