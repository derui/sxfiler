open Sxfiler_types.Types

(** {!Layout} defines type to preset of layout *)
module Layout = struct
  include Layout

  type js = Js.number

  let to_js : t -> js Js.t = fun t -> Js.number_of_float @@ float_of_int @@ to_enum t
  let of_js : js Js.t -> t = fun js -> match of_enum @@ int_of_float @@ Js.float_of_number js with
    | None -> failwith "Unknown type"
    | Some v -> v
end

(** {!Source_type} defines type of source for completion.  *)
module Source_type = struct
  include Source_type

  type js = Js.number

  let to_js : t -> js Js.t = fun t -> Js.number_of_float @@ float_of_int @@ to_enum t
  let of_js : js Js.t -> t = fun js -> match of_enum @@ int_of_float @@ Js.float_of_number js with
    | None -> failwith "Unknown type"
    | Some v -> v
end

module Sort_type = struct
  include Sort_type

  type js = Js.number
  let to_js : t -> js Js.t = fun t -> Js.number_of_float @@ float_of_int @@ to_enum t
  let of_js : js Js.t -> t = fun js -> match of_enum @@ int_of_float @@ Js.float_of_number js with
    | None -> failwith "Unknown type"
    | Some v -> v
end

module Candidate = struct
  class type js = object
    method start: int Js.readonly_prop
    method length: int Js.readonly_prop
    method value: < > Js.t Js.readonly_prop
  end
end
