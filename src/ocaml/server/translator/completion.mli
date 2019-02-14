module Item : sig
  type t =
    { id : string
    ; value : string }
end

module Candidates : sig
  type candidate =
    { start : int
    ; length : int
    ; value : Item.t }

  type t = candidate list [@@deriving yojson]

  include
    Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Completion.candidates
end

module Collection : sig
  type t = Item.t list [@@deriving yojson]

  include
    Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Completion.collection
end
