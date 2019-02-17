module D = Sxfiler_domain.Types

module Sort_type = struct
  type t =
    | Name [@name "name"]
        | Size [@name "size"]
        | Date [@name "date"]
  [@@deriving show, yojson]

  let to_domain = function
    | Name -> D.Sort_type.Name
    | Date -> D.Sort_type.Date
    | Size -> D.Sort_type.Size

  let of_domain = function
    | D.Sort_type.Name -> Name
    | D.Sort_type.Date -> Date
    | D.Sort_type.Size -> Size
end
