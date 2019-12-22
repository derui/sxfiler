module C = Sxfiler_domain.Completion
module G = Sxfiler_server_generated

module Item = struct
  type t = G.Completion.Item.t

  let to_domain (t : t) = { C.Item.id = t.id; value = t.value }
  let of_domain (t : C.Item.t) = { G.Completion.Item.id = t.C.Item.id; value = t.value }
end

module Candidate = struct
  type t = G.Completion.Candidate.t

  let to_domain (t : t) =
    { C.Candidate.start = t.start; length = t.length; value = Option.get t.value |> Item.to_domain }

  let of_domain (t : C.Candidate.t) =
    {
      G.Completion.Candidate.start = t.C.Candidate.start;
      length = t.length;
      value = Item.of_domain t.value |> Option.some;
    }
end

module Candidates = struct
  let to_domain t = List.map Candidate.to_domain t
  let of_domain t = List.map Candidate.of_domain t
end

module Collection = struct
  let to_domain t = List.map Item.to_domain t
  let of_domain t = List.map Item.of_domain t
end
