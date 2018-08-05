module C = Sxfiler_completion

module Item = struct
  type t = {
    id: string;
    value: string
  } [@@deriving yojson,show]

  let to_domain t = {
    C.Domain.Item.id = t.id;
    value = t.value;
  }

  let of_domain t = {
    id = t.C.Domain.Item.id;
    value = t.value;
  }
end

module Candidate = struct
  type t = {
    start: int;
    length: int;
    value: Item.t;
  } [@@deriving yojson,show]

  let to_domain t = {
    C.Domain.Candidate.start = t.start;
    length = t.length;
    value = Item.to_domain t.value;
  }

  let of_domain t = {
    start = t.C.Domain.Candidate.start;
    length = t.length;
    value = Item.of_domain t.value;
  }
end
