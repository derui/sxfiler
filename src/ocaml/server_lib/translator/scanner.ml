open Sxfiler_core
open Sxfiler_rpc.Types.Scanner
module D = Sxfiler_domain.Scanner

let to_yojson t =
  `Assoc [
    "id", `String t.id;
    "location", `String t.location;
    "nodes", `List (List.map Node.to_yojson t.nodes);
    "history", Location_history.to_yojson t.history;
  ]

let of_yojson js =
  let open Yojson.Safe.Util in
  try
    let id = js |> member "id" |> to_string
    and location = js |> member "location" |> to_string
    and nodes = js |> member "nodes" |> to_list
    and history = js |> member "history" |> Location_history.of_yojson in
    let open Result.Infix in
    let nodes = List.fold_left (fun accum node ->
        accum >>= fun accum ->
        Node.of_yojson node >>= fun node ->
        Ok (node :: accum)
      ) (Ok [])
        nodes
    in
    nodes >>= fun nodes ->
    history >>= fun history ->
    Ok {id;location;nodes = List.rev nodes;history}
  with Type_error (s, _) -> Error s

let of_domain t = {
  id = t.D.id;
  location = Path.to_string t.location;
  nodes = List.map Node.of_domain t.nodes;
  history = Location_history.of_domain t.history;
}
