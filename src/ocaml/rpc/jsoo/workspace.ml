module T = Sxfiler_types
module Tj = Sxfiler_types_jsoo
module Rpc = Sxfiler_rpc

module Make_sync = struct
  open Rpc.Workspace.Make_sync
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
  open Rpc.Workspace.Get_sync
  module Jsoo = struct
    class type params = object
      method name: Js.js_string Js.t Js.readonly_prop
    end
  end

  let params_to_json t = object%js
    val name = Js.string t.name
  end

  let result_of_json : Tj.Workspace.js Js.t -> result = Tj.Workspace.of_js
end
