open Sxfiler_core
open Sxfiler_rpc.Types.Plan
module D = Sxfiler_domain.Plan

let node_list_to_yojson t =
  `Assoc [("operation", `Int (Operation.to_int t.operation)); ("node", Node.to_yojson t.node)]

let node_list_of_yojson js =
  let open Yojson.Safe.Util in
  try
    let operation = js |> member "operation" |> to_int |> Operation.of_int |> Option.get_exn
    and node = js |> member "node" |> Node.of_yojson in
    let open Result.Infix in
    node >|= fun node -> {operation; node}
  with Type_error (s, _) -> Error s

let node_list_of_domain t = {operation = t.D.operation; node = Node.of_domain t.D.node}

let to_yojson t =
  `Assoc
    [ ("source", `List (List.map node_list_to_yojson t.source))
    ; ("dest", `List (List.map node_list_to_yojson t.dest)) ]

let of_yojson js =
  let open Yojson.Safe.Util in
  try
    let source = js |> member "source" |> convert_each node_list_of_yojson
    and dest = js |> member "dest" |> convert_each node_list_of_yojson in
    let open Result.Infix in
    let conv_list list =
      List.fold_left (fun list v -> list >>= fun list -> v >|= fun v -> v :: list) (Ok []) list
      >>= Result.lift List.rev
    in
    conv_list source >>= fun source -> conv_list dest >>= fun dest -> Ok {source; dest}
  with Type_error (s, _) -> Error s

let of_domain t =
  {source = List.map node_list_of_domain t.D.source; dest = List.map node_list_of_domain t.D.dest}
