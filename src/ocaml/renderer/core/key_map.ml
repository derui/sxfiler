type module_type = Types.Viewer_module.t

module Key_map = Map.Make(struct
    type t = module_type
    let compare = Pervasives.compare
  end)

type t = Key_bindings.t Key_map.t

let empty = Key_map.empty

let add key_map ~module_type ~bindings = Key_map.add module_type bindings key_map
let remove key_map ~module_type = Key_map.remove module_type key_map

let find key_map ~module_type = Key_map.find_opt module_type key_map

let dump = Key_map.bindings

class type binding = object
  method action: Js.js_string Js.t Js.readonly_prop
  method key: Js.js_string Js.t Js.readonly_prop
end

let to_json t =
  let dumped = dump t
               |> List.map (fun (module_type, bindings) ->
                   (module_type, Key_bindings.dump bindings)) in
  let obj_values =
    dumped
    |> List.map (fun (module_type, bindings) ->
        (* convert binding to (string, js_array Js.t) *)
        let key = Types.Viewer_module.to_string module_type in
        let array = bindings
                    |> List.map (fun (key, action) ->
                        object%js
                          val action = Js.string @@ Callable_action.to_string action
                          val key = Js.string @@ key
                        end
                      )
                    |> List.map Js.Unsafe.inject
                    |> Array.of_list
                    |> Js.array
                    |> Js.Unsafe.inject in
        (key, array)
      )
    |> Array.of_list in
  Js.Unsafe.obj obj_values

let of_json js =
  let keys = Js.object_keys js in
  let keybindings_per_viewer = Js.to_array @@ Js.array_map (fun key ->
      let array : binding Js.t array = Js.to_array @@ Js.Unsafe.get js key in
      (Js.to_string key, Array.fold_left (fun binding obj ->
           let action = Callable_action.of_string Js.(to_string obj##.action) in
           Key_bindings.add binding ~key:Js.(to_string obj##.key) ~action
         ) Key_bindings.empty array
      )
    ) keys
  in
  Array.fold_left (fun keymap (modtype, bindings) ->
      add keymap ~module_type:Types.Viewer_module.(of_string modtype) ~bindings:bindings
    ) empty keybindings_per_viewer
