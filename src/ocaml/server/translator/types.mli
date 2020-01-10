module Sort_type : sig
  open Sxfiler_server_generated.Types

  include
    Core.Domain_translator
      with type t := SortType.t
       and type domain := Sxfiler_domain.Types.Sort_type.t
end
