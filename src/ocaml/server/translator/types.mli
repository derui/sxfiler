module Sort_type : sig
  type t =
    | Name [@name "name"]
        | Size [@name "size"]
        | Date [@name "date"]
  [@@deriving yojson]

  include
    Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Types.Sort_type.t
end
