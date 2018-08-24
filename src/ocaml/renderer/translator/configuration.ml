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

  let of_domain t = match t with
      | D.Types.Sort_type.Size -> 1
      | D.Types.Sort_type.Name -> 2
      | D.Types.Sort_type.Date -> 3

  let to_domain t = match t with
    | 1 -> D.Types.Sort_type.Size
    | 2 -> D.Types.Sort_type.Name
    | 3 -> D.Types.Sort_type.Date
    | _ -> failwith "Unknown type"
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
