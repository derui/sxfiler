module Sort_type : sig
  open Sxfiler_generated.Types

  include
    Core.Domain_translator
      with type t := SortType.t
       and type domain := Sxfiler_domain.Types.Sort_type.t
       and type error := string
end
