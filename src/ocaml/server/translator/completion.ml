module C = Sxfiler_domain.Completion

module Item = struct
  type t =
    { id : string
    ; value : string }
  [@@deriving yojson]

  let to_domain (t : t) = {C.Item.id = t.id; value = t.value}
  let of_domain (t : C.Item.t) = {id = t.C.Item.id; value = t.value}
end

module Candidates = struct
  type candidate =
    { start : int
    ; length : int
    ; value : Item.t }
  [@@deriving yojson]

  type t = candidate list [@@deriving yojson]

  let candidate_to_domain t =
    {C.Candidate.start = t.start; length = t.length; value = Item.to_domain t.value}

  let candidate_of_domain t =
    {start = t.C.Candidate.start; length = t.length; value = Item.of_domain t.value}

  let to_domain t = List.map candidate_to_domain t
  let of_domain t = List.map candidate_of_domain t
end

module Collection = struct
  type t = Item.t list [@@deriving yojson]

  let to_domain t = List.map Item.to_domain t
  let of_domain t = List.map Item.of_domain t
end