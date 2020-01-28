module D = Sxfiler_domain.Types
module G = Sxfiler_generated

module Sort_type = struct
  let of_domain = function D.Sort_type.Name -> G.Types.SortType.NAME | Date -> DATE | Size -> SIZE

  let to_domain = function G.Types.SortType.NAME -> Ok D.Sort_type.Name | DATE -> Ok Date | SIZE -> Ok Size
end
