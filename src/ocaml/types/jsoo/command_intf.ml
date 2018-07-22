open Sxfiler_types.Command_intf

(** Class is type of command and parameter set. *)
module Class = struct
  include Class

  type js = Js.number

  let to_js t = Js.number_of_float @@ float_of_int @@ to_enum t
  let of_js js = match of_enum @@ int_of_float @@ Js.float_of_number js with
    | None -> failwith "Unknown type"
    | Some v -> v

end

(** {!Param_type} is type of parameter to be able to give value from other. *)
module Param_type = struct
  include Param_type

  type js = Js.number
  let to_js t = Js.number_of_float @@ float_of_int @@ to_enum t
  let of_js js = match of_enum @@ int_of_float @@ Js.float_of_number js with
    | None -> failwith "Unknown param type"
    | Some v -> v
end

module Param_def = struct
  include Param_def

  class type js = object
    method name: Js.js_string Js.t Js.readonly_prop
    method typ: Param_type.js Js.t Js.readonly_prop
  end

  let to_js t = object%js
    val name = Js.string t.name
    val typ = Param_type.to_js t.typ
  end

  let of_js js = {
    name = Js.to_string js##.name;
    typ = Param_type.of_js js##.typ;
  }
end

class type js = object
  method commandClass: Class.js Js.t Js.readonly_prop
  method paramDefs: Param_def.js Js.t Js.js_array Js.t Js.readonly_prop
end

let to_js t = object%js
  val commandClass = Class.to_js t.command_class
  val paramDefs = List.map Param_def.to_js t.param_defs |> Array.of_list |> Js.array
end

let of_js js = {
  command_class = Class.of_js js##.commandClass;
  param_defs = Js.to_array js##.paramDefs |> Array.to_list |> List.map Param_def.of_js;
}
