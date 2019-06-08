module D = Sxfiler_domain

type key = string
type action = string
type bind = key * action

(* make fixture for node *)
let fixture bindings =
  let bindings = List.map (fun (key, action) -> (Sxfiler_kbd.make key, action)) bindings in
  List.fold_left
    (fun keymap (key, value) -> D.Key_map.add ~contexts:[] ~key ~value keymap)
    (D.Key_map.make ()) bindings
