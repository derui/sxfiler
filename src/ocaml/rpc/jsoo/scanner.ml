module T = Sxfiler_domain
module Tj = Sxfiler_domain_jsoo
module Rpc = Sxfiler_rpc

module Make_sync = struct
  open Rpc.Scanner.Make_sync
  module Jsoo = struct
    class type params = object
      method initialLocation: Js.js_string Js.t Js.readonly_prop
      method name: Js.js_string Js.t Js.readonly_prop
    end

  end

  let params_to_json t = object%js
    val initialLocation = Js.string t.initial_location
    val name = Js.string t.name
  end
end

module Get_sync = struct
  open Rpc.Scanner.Get_sync
  module Jsoo = struct
    class type params = object
      method name: Js.js_string Js.t Js.readonly_prop
    end
  end

  let params_to_json t = object%js
    val name = Js.string t.name
  end

  let result_of_json : Tj.Scanner.js Js.t -> result = Tj.Scanner.of_js
end
