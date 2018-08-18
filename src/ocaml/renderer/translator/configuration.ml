(** This module provides function that is used to convert json to OCaml type,
    and extend original modules.
*)
open Sxfiler_domain.Configuration

class type js = object
  method sortOrder: Types.Sort_type.js Js.t Js.readonly_prop
end

let of_js : js Js.t -> t = fun js ->
  {
    sort_order = Types.Sort_type.of_js js##.sortOrder;
  }

let to_js t : js Js.t = object%js
  val sortOrder = Types.Sort_type.to_js t.sort_order
end
