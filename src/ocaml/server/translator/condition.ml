module D = Sxfiler_domain.Condition

type t = { contexts : string list }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

let of_domain t =
  let module T = Sxfiler_domain in
  { contexts = T.Condition.to_list t }

let to_domain t =
  let empty = D.empty in
  List.fold_left (fun cond context -> D.enable cond ~context) empty t.contexts
