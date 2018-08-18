open Sxfiler_domain.Types

module Sort_type = struct
  include Sort_type

  type js = Js.number
  let to_js : t -> js Js.t = fun t -> Js.number_of_float @@ float_of_int (match t with
      | Size -> 1
      | Name -> 2
      | Date -> 3)
  let of_js : js Js.t -> t = fun js -> match int_of_float @@ Js.float_of_number js with
    | 1 -> Size
    | 2 -> Name
    | 3 -> Date
    | _ -> failwith "Unknown type"
end
