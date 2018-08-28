open Sxfiler_core
open Sxfiler_rpc.Types.Tree_snapshot
module D = Sxfiler_domain.Tree_snapshot

let to_yojson t =
  `Assoc [("directory", `String t.directory); ("nodes", `List (List.map Node.to_yojson t.nodes))]


let of_yojson js =
  let open Yojson.Safe.Util in
  try
    let directory = js |> member "directory" |> to_string
    and nodes = js |> member "nodes" |> to_list in
    let open Result.Infix in
    let nodes =
      List.fold_left
        (fun accum node ->
           accum >>= fun accum -> Node.of_yojson node >>= fun node -> Ok (node :: accum) )
        (Ok []) nodes
    in
    nodes >>= fun nodes -> Ok {directory; nodes = List.rev nodes}
  with Type_error (s, _) -> Error s


(** [of_domain t] converts {!type:D.t} to {!type:t}. *)
let of_domain t = {directory = t.D.directory; nodes = List.map Node.of_domain t.nodes}
