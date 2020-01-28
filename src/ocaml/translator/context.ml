open Sxfiler_core
module D = Sxfiler_domain.Context
module G = Sxfiler_generated

type error = unit

let of_domain (t : D.t) = D.to_list t

let to_domain (t : string list) =
  let empty = D.empty in
  List.fold_left (fun cond context -> D.enable cond ~context) empty t |> Result.ok
