module Item : sig
  type t =
    { id : string
    ; value : string }
  [@@deriving show]
end

module Candidate : sig
  type t =
    { start : int
    ; length : int
    ; value : Item.t }
  [@@deriving show]
end

module Candidates : sig
  type t = Candidate.t list [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  include
    Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Completion.candidates
end

module Collection : sig
  type t = Item.t list [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

  include
    Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Completion.collection
end
