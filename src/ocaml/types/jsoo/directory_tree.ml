include Sxfiler_types.Directory_tree

(** From server, simple json object that keys are parent directory and value is
    array obj object.
*)
type js = < >

let of_js : js Js.t -> t = fun js ->
  let rec convert obj =
    let dir = Js.object_keys obj in
    let dir = Js.array_get dir 0 in
    let dir = Js.Optdef.map dir Js.to_string in
    let dir = Js.Optdef.get dir (fun () -> "") in
    let children : < > Js.t Js.js_array Js.t = Js.Unsafe.get obj (Js.string dir) in
    Tree (dir, Js.array_map convert children |> Js.to_array |> Array.to_list)
  in

  (* if empty object, as nil. *)
  let keys = Js.object_keys js in
  if keys##.length = 0 then Nil
  else convert js
