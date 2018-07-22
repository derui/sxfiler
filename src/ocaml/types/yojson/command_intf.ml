open Sxfiler_types.Command_intf

module Class = struct
  include Class

  let to_yojson: t -> Yojson.Safe.json = fun t -> `Int (to_enum t)
  let of_yojson = function
    | `Int v -> begin match of_enum v with
        | None -> Error "Unknown Command class"
        | Some v -> Ok v
      end
    | _ -> Error "Command class should be int"
end

module Param_type = struct
  include Param_type

  let to_yojson: t -> Yojson.Safe.json = fun t -> `Int (to_enum t)
  let of_yojson = function
    | `Int v -> begin match of_enum v with
        | None -> Error "Unknown Param type "
        | Some v -> Ok v
      end
    | _ -> Error "Param type should be int"
end

module Param_def = struct
  include Param_def

  module Js = struct
    type t = {
      name: string;
      typ: Param_type.t;
    } [@@deriving yojson]
  end

  let to_yojson: t -> Yojson.Safe.json = fun t -> Js.to_yojson {
      Js.name = t.name;
      typ = t.typ;
    }

  let of_yojson js =
    let open Ppx_deriving_yojson_runtime in
    Js.of_yojson js >>= fun v ->
    Ok {
      name = v.Js.name;
      typ = v.Js.typ;
    }
end

module Js = struct
  type t = {
    command_class: Class.t;
    param_defs: Param_def.t list;
  }
  [@@deriving yojson]
end

let to_yojson t = Js.to_yojson {
    Js.command_class = t.command_class;
    param_defs = t.param_defs;
  }

let of_yojson js =
  let open Ppx_deriving_yojson_runtime in
  Js.of_yojson js >>= fun v ->
  Ok {
    command_class = v.Js.command_class;
    param_defs = v.Js.param_defs;
  }
