(** This module provides function that is used to convert json to OCaml type,
    and extend original modules.
*)
open Sxfiler_rpc.Types.Configuration
module D = Sxfiler_domain

module Sort_type = struct
  include Sort_type

  type js = Js.number

  let to_js t = Js.number_of_float @@ float_of_int t
  let of_js js = Js.float_of_number js |> int_of_float

  let of_domain t = D.Types.Sort_type.to_int t

  let to_domain t = let open Sxfiler_core in
    Option.get_exn @@ D.Types.Sort_type.of_int t
end

class type js = object
  method defaultSortOrder: Sort_type.js Js.t Js.readonly_prop
end

let of_js : js Js.t -> t = fun js ->
  {
    default_sort_order = Sort_type.of_js js##.defaultSortOrder;
  }

let to_js t : js Js.t = object%js
  val defaultSortOrder = Sort_type.to_js t.default_sort_order
end
