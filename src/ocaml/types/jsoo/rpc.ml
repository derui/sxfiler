module W = Workspace
open Sxfiler_types.Rpc

module Rpc_ompletion = struct
  module Setup_file_sync = struct
    open Rpc_completion.Setup_file_sync

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

    type result = Types.Candidate.js Js.t Js.js_array
  end

  module Read_file_sync = struct
    open Rpc_completion.Read_file_sync
    module Jsoo = Read_common_js

    let params_to_json t = object%js
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

  module Read_directory_sync = struct
    open Rpc_completion.Read_directory_sync
    module Jsoo = Read_common_js

    let params_to_json t = object%js
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

  module Read_history_sync = struct
    open Rpc_completion.Read_history_sync
    module Jsoo = Read_common_js

    let params_to_json t = object%js
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

module Rpc_workspace = struct
  module Make_sync = struct
    open Rpc_workspace.Make_sync
    module Jsoo = struct
      class type params = object
        method initialDirectory: Js.js_string Js.t Js.readonly_prop
        method name: Js.js_string Js.t Js.readonly_prop
      end

      class type result = object
        method created: bool Js.t Js.readonly_prop
      end
    end

    let params_to_jsonon t = object%js
      val initialDirectory = Js.string t.initial_directory
      val name = Js.string t.name
    end

    let result_of_json : Jsoo.result Js.t -> result = fun js -> {
      created = Js.to_bool js##.created;
    }
  end

  module Get_sync = struct
    open Rpc_workspace.Get_sync
    module Jsoo = struct
      class type params = object
        method name: Js.js_string Js.t Js.readonly_prop
      end
    end

    let params_to_json t = object%js
      val name = Js.string t.name
    end

    let result_of_json : W.js Js.t -> result = W.of_js
  end
end

module Rpc_notification = struct
  module Workspace_update = struct
    open Rpc_notification.Workspace_update

    class type js_params = object
      method name: Js.js_string Js.t Js.readonly_prop
      method workspace: Workspace.js Js.t Js.readonly_prop
    end

    let params_of_json js = {
      name = Js.to_string js##.name;
      workspace = Workspace.of_js js##.workspace;
    }
  end
end
