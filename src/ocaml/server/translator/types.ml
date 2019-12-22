module D = Sxfiler_domain.Types
module G = Sxfiler_server_generated

module Sort_type = struct
  let to_domain = function G.Types.SortType.Name -> D.Sort_type.Name | Date -> Date | Size -> Size
  let of_domain = function D.Sort_type.Name -> G.Types.SortType.Name | Date -> Date | Size -> Size
end
