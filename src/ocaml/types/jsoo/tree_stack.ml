include Sxfiler_types.Tree_stack

(* json representation of server uses ppx_deriving_yojson, so this definition
   covers it.
*)

type js = < > Js.t Js.js_array Js.t Js.js_array

let array_to_data : < > Js.t Js.js_array Js.t -> data = fun ary ->
  let constr = Js.array_get ary 0
               |> Util.flip Js.Optdef.map Js.Unsafe.coerce
               |> Util.flip Js.Optdef.map Js.to_string
               |> Js.Optdef.to_option in
  let value = Js.array_get ary 1 |> Js.Optdef.to_option in
  match constr, value with
  | Some "Snapshot", Some value -> Snapshot (Tree_snapshot.of_js @@ Js.Unsafe.coerce value)
  | Some "Directory_tree", Some value -> Directory_tree (Directory_tree.of_js @@ Js.Unsafe.coerce value)
  | _, _ -> failwith "Unknown representation"

let of_js : js Js.t -> t = fun js ->
  Js.array_map array_to_data js |> Js.to_array |> Array.to_list
