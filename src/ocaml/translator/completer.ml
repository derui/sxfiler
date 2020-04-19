open Sxfiler_core
module C = Sxfiler_domain.Completer
module G = Sxfiler_generated.Completer

type all_error = Invalid_item

module Item = struct
  type t = G.Completer.Item.t

  let to_domain (t : t) = Ok (C.Item.make ~id:t.id ~value:t.value)

  let of_domain (t : C.Item.t) = { G.Completer.Item.id = C.Item_id.value t.C.Item.id; value = t.value }
end

module Candidate = struct
  type t = G.Completer.Candidate.t

  let to_domain (t : t) =
    let open Result.Infix in
    let* item = Option.to_result ~none:Invalid_item t.value in
    let* item = Item.to_domain item in
    C.Candidate.make ~start:t.start ~length:t.length ~value:item |> Result.ok

  let of_domain (t : C.Candidate.t) =
    {
      G.Completer.Candidate.start = t.C.Candidate.start;
      length = t.length;
      value = Item.of_domain t.value |> Option.some;
    }
end

module Candidates = struct
  type error = all_error

  let to_domain t =
    let open Result.Infix in
    let* list =
      List.fold_left
        (fun l v ->
          let* l = l in
          let* v = Candidate.to_domain v in
          Ok (v :: l))
        (Ok []) t
    in
    List.rev list |> Result.ok

  let of_domain t = List.map Candidate.of_domain t
end

module Collection = struct
  type error = all_error

  let to_domain t =
    let open Result.Infix in
    let* list =
      List.fold_left
        (fun l v ->
          let* l = l in
          let* v = Item.to_domain v in
          Ok (v :: l))
        (Ok []) t
    in
    List.rev list |> Result.ok

  let of_domain t = List.map Item.of_domain t
end
