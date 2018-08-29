module D = Sxfiler_domain.Key_map
module T = Sxfiler_rpc.Types.Key_map

(* translator between json and response/request type *)
let key_to_yojson t =
  `Assoc
    [ ("key", `String t.T.key)
    ; ("action", `String t.action)
    ; ("when", Condition.to_yojson t.condition) ]

let key_of_yojson js =
  let open Yojson.Safe.Util in
  try
    let key = js |> member "key" |> to_string
    and action = js |> member "action" |> to_string
    and condition = js |> member "when" in
    let open Sxfiler_core.Result.Infix in
    Condition.of_yojson condition >>= fun condition -> Ok {T.key; action; condition}
  with Type_error (s, _) -> Error s

let to_yojson t = `Assoc [("bindings", `List (List.map key_to_yojson t.T.bindings))]

let of_yojson js =
  let open Yojson.Safe.Util in
  try
    let bindings = js |> member "bindings" |> to_list in
    let bindings =
      List.fold_left
        (fun accum key ->
           match (key_of_yojson key, accum) with
           | Ok key, Ok accum -> Ok (key :: accum)
           | (Error _ as v), _ -> v
           | _, Error _ -> accum )
        (Ok []) bindings
    in
    Sxfiler_core.Result.fmap ~f:(fun v -> {T.bindings = List.rev v}) bindings
  with Type_error (s, _) -> Error s

(* translator between domain and response/request type *)
let of_domain t =
  let bindings = D.bindings t in
  List.map
    (fun (cond, key, value) ->
       {T.condition = Condition.of_domain cond; key = Sxfiler_kbd.to_keyseq key; action = value} )
    bindings
  |> fun v -> {T.bindings = v}

let to_domain t =
  let empty = D.make () in
  List.fold_left
    (fun keymap binding ->
       match Sxfiler_kbd.of_keyseq binding.T.key with
       | None -> keymap
       | Some key ->
         D.add keymap
           ~condition:(Condition.to_domain binding.condition)
           ~key ~value:binding.action )
    empty t.T.bindings
