module D = Sxfiler_domain.Key_map
module Gen = Sxfiler_server_generated

(* translator between domain and response/request type *)
let of_domain (t : D.t) =
  let bindings = D.bindings t in
  List.map
    (fun (contexts, key, value) ->
      { Gen.Keymap.Binding.contexts; key = Sxfiler_kbd.to_keyseq key; action = value })
    bindings

let to_domain (t : Gen.Keymap.Keymap.t) =
  let empty = D.make () in
  List.fold_left
    (fun keymap (binding : Gen.Keymap.Binding.t) ->
      match Sxfiler_kbd.of_keyseq binding.key with
      | None -> keymap
      | Some key -> D.add keymap ~contexts:binding.contexts ~key ~value:binding.action)
    empty t
