open Sxfiler_types.Completion

module Candidate = struct
  class type js = object
    method start: int Js.readonly_prop
    method length: int Js.readonly_prop
    method value: < > Js.t Js.readonly_prop
  end
end

(** Common_item is useful type to complete by RPC with unknown types. *)
module Common_item = struct
  include Common_item
  class type js = object
    method id: Js.js_string Js.t Js.readonly_prop
    method value: Js.js_string Js.t Js.readonly_prop
  end

  let to_js: t -> js Js.t = fun t -> object%js
    val id = Js.string t.id
    val value = Js.string t.value
  end
end
