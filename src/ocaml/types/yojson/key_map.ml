(* [Key_map] module should be able to convert between json and ocaml.  *)
include Sxfiler_types.Key_map

let to_js : t -> Yojson.Safe.json = fun t ->
  let module CA = Sxfiler_types.Callable_action in
  let key_value = dump t |> List.map (fun (k, v) -> (k, CA.to_string v)) in
  `Assoc (List.map (fun (k, v) -> (k, `String v)) key_value)

let of_js : Yojson.Safe.json -> t = fun json ->
  let module CA = Sxfiler_types.Callable_action in
  match json with
  | `Assoc values -> List.fold_left (fun keymap (key, value) ->
      let value = Yojson.Safe.Util.to_string value in
      add keymap ~key ~action:(CA.of_string value)
    ) empty values
  | _ -> Printf.printf "Invalid keymap"; empty
