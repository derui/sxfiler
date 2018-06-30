open Sxfiler_types.Rpc

module Completion = struct
  module Setup = struct
    include Completion.Setup

    class type js_param = object
      method sourceType: Js.js_string Js.t Js.readonly_prop
    end

    let param_to_js : param -> js_param Js.t = fun t -> object%js
      val sourceType = Types.Source_type.to_js t.source_type
    end
  end

  module Read_common_js = struct
    class type param = object
      method input: Js.js_string Js.t Js.readonly_prop
    end

    type result = Types.Candidate.js Js.t Js.js_array
  end

  module Read_file = struct
    include Completion.Read_file
    module Jsoo = Read_common_js

    let param_to_js t = object%js
      val input = Js.string t.input
    end

    let result_of_json : Jsoo.result Js.t -> result = fun t ->
      let module T = Sxfiler_types.Types in
      Js.to_array @@ Js.array_map (fun v -> T.Candidate.{
          start = v##.start;
          length = v##.length;
          value = Node.of_js @@ Js.Unsafe.coerce v##.value;
        }) t
  end

  module Read_directory = struct
    include Completion.Read_directory
    module Jsoo = Read_common_js

    let param_to_js t = object%js
      val input = Js.string t.input
    end

    let result_of_json : Jsoo.result Js.t -> result = fun t ->
      let module T = Sxfiler_types.Types in
      Js.to_array @@ Js.array_map (fun v -> T.Candidate.{
          start = v##.start;
          length = v##.length;
          value = Directory_tree.of_js @@ Js.Unsafe.coerce v##.value;
        }) t
  end

  module Read_history = struct
    include Completion.Read_history
    module Jsoo = Read_common_js

    let param_to_js t = object%js
      val input = Js.string t.input
    end

    let result_of_json : Jsoo.result Js.t -> result = fun t ->
      let module T = Sxfiler_types.Types in
      Js.to_array @@ Js.array_map (fun v -> T.Candidate.{
          start = v##.start;
          length = v##.length;
          value = Snapshot_record.of_js @@ Js.Unsafe.coerce v##.value;
        }) t
  end
end
