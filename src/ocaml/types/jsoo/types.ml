open Sxfiler_types.Types

(** {!Layout} defines type to preset of layout *)
module Layout = struct
  include Layout

  type js = Js.js_string

  let to_js : t -> js Js.t = fun t -> Js.string @@ to_string t
  let of_js : js Js.t -> t = fun js -> of_string @@ Js.to_string js
end

(** {!Source_type} defines type of source for completion.  *)
module Source_type = struct
  include Source_type

  type js = Js.js_string

  let to_js : t -> js Js.t = fun t -> Js.string @@ to_string t
  let of_js : js Js.t -> t = fun js -> of_string @@ Js.to_string js
end

module Candidate = struct
  class type js = object
    method start: int Js.readonly_prop
    method length: int Js.readonly_prop
    method value: < > Js.t Js.readonly_prop
  end
end
