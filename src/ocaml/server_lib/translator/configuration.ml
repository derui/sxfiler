module D = Sxfiler_domain
module T = Sxfiler_rpc.Types

module Sort_type = struct
  open T.Configuration.Sort_type

  let to_yojson t = `Int t

  let of_yojson js : (t, string) result =
    let open Yojson.Safe.Util in
    try Ok (js |> to_int) with Type_error (s, _) -> Error s


  let to_domain = function
    | 1 ->
      D.Types.Sort_type.Date
    | 2 ->
      D.Types.Sort_type.Name
    | 3 ->
      D.Types.Sort_type.Size
    | _ ->
      failwith "Unknown sort_type"


  let of_domain = function
    | D.Types.Sort_type.Date ->
      1
    | D.Types.Sort_type.Name ->
      2
    | D.Types.Sort_type.Size ->
      3
end

let to_yojson t : Yojson.Safe.json =
  `Assoc [("defaultSortOrder", Sort_type.to_yojson t.T.Configuration.default_sort_order)]


let of_yojson js =
  let open Yojson.Safe.Util in
  try
    let sort_order = js |> member "defaultSortOrder" in
    let open Sxfiler_core.Result.Infix in
    Sort_type.of_yojson sort_order
    >>= fun default_sort_order -> Ok {T.Configuration.default_sort_order}
  with Type_error (s, _) -> Error s


let of_domain t =
  {T.Configuration.default_sort_order = Sort_type.of_domain t.D.Configuration.default_sort_order}


let to_domain t =
  {D.Configuration.default_sort_order = Sort_type.to_domain t.T.Configuration.default_sort_order}
