include Sxfiler_types.Key_map

let to_js : t -> Yojson.json = fun t ->
  let module CA = Sxfiler_types.Callable_action in
  let key_value = dump t |> List.map (fun (k, v) -> (k, CA.to_string v)) in
  `Assoc (List.map (fun (k, v) -> (k, `String v)) key_value)
