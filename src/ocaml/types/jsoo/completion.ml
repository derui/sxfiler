open Sxfiler_types.Completion

(** {!Source_class} defines type of source for completion.  *)
module Source_class = struct
  include Source_class

  type js = Js.number

  let to_js : t -> js Js.t = fun t -> Js.number_of_float @@ float_of_int @@ to_enum t
  let of_js : js Js.t -> t = fun js -> match of_enum @@ int_of_float @@ Js.float_of_number js with
    | None -> failwith "Unknown type"
    | Some v -> v
end

module Item = struct
  include Item
  class type js = object
    method id: Js.js_string Js.t Js.readonly_prop
    method value: Js.js_string Js.t Js.readonly_prop
  end

  let to_js: t -> js Js.t = fun t -> object%js
    val id = Js.string t.id
    val value = Js.string t.value
  end

  let of_js: js Js.t -> t = fun js -> {
      id = Js.to_string js##.id;
      value = Js.to_string js##.value;
    }
end

module Candidate = struct
  include Candidate
  class type js = object
    method start: int Js.readonly_prop
    method length: int Js.readonly_prop
    method value: Item.js Js.t Js.readonly_prop
  end

  let to_js: t -> js Js.t = fun t -> object%js
    val start = t.start
    val length = t.length
    val value = Item.to_js t.value
  end

  let of_js: js Js.t -> t = fun js -> {
      start = js##.start;
      length = js##.length;
      value = Item.of_js js##.value;
    }
end
