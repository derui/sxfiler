module D = Sxfiler_domain.Condition
module G = Sxfiler_server_generated

let of_domain (t : D.t) = D.to_list t

let to_domain (t : string list) =
  let empty = D.empty in
  List.fold_left (fun cond context -> D.enable cond ~context) empty t
