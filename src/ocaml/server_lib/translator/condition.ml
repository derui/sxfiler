module D = Sxfiler_domain.Condition
open Sxfiler_rpc.Types.Condition

let to_yojson t : Yojson.Safe.json = `List (List.map (fun v -> `String v) t)

let of_yojson js : (t, string) result =
  let open Yojson.Safe.Util in
  try
    let enabled_contexts = js |> to_list in
    let enabled_contexts = List.map to_string enabled_contexts in
    Ok enabled_contexts
  with Type_error (s, _) -> Error s


let of_domain t =
  let module T = Sxfiler_domain in
  T.Condition.to_list t


let to_domain t =
  let empty = D.empty in
  List.fold_left (fun cond context -> D.enable cond ~context) empty t
