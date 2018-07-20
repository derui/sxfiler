
module Original_key_binding = struct
  type t = {
    key: string;
    action: Callable_action.t;
    condition: Types.Condition.t;
  }

  class type js = object
    method key: Js.js_string Js.t Js.readonly_prop
    method action: Js.js_string Js.t Js.readonly_prop
    method _when: Types.Condition.js Js.t Js.readonly_prop
  end

  let to_js : t -> js Js.t = fun t -> object%js
    val key = Js.string t.key
    val action = Js.string @@ Callable_action.to_string t.action
    val _when = Types.Condition.to_js t.condition
  end

  let of_js : js Js.t -> t = fun js -> {
    key = Js.to_string js##.key;
    action = Callable_action.of_string @@ Js.to_string js##.action;
    condition = Types.Condition.of_js js##._when;
  }
end

module Key_map = Map.Make(struct
    type t = Types.Condition.t
    let compare = Pervasives.compare
  end)

type t = Key_bindings.t Key_map.t

let empty = Key_map.empty

let add t ~condition ~bindings = Key_map.add condition bindings t
let remove t ~condition = Key_map.remove condition t

let find t ~condition = Key_map.find_opt condition t

let dump = Key_map.bindings

let to_json : t -> < > Js.t = fun t ->
  let dumps = dump t in
  List.map (fun (condition, bindings) ->
      let bindings = Key_bindings.dump bindings in
      List.map (fun (key, action) ->
          Original_key_binding.to_js {
            key;
            action;
            condition;
          }
        ) bindings
    ) dumps
  |> Array.of_list
  |> Js.array
  |> Js.Unsafe.coerce

let of_json : < > Js.t -> t = fun js ->
  let js = Js.Unsafe.coerce js in
  let original_bindings = Array.to_list @@ Js.to_array @@ Js.array_map Original_key_binding.of_js js in
  List.fold_left (fun key_map binding ->
      let condition = binding.Original_key_binding.condition in
      match find ~condition key_map with
      | None -> let new_bindings = Key_bindings.empty in
        let new_bindings = Key_bindings.add ~key:binding.Original_key_binding.key
            ~action:binding.Original_key_binding.action new_bindings in
        add ~condition ~bindings:new_bindings key_map
      | Some bindings ->
        let bindings = Key_bindings.add ~key:binding.Original_key_binding.key
            ~action:binding.Original_key_binding.action bindings in
        add ~condition ~bindings key_map
    ) Key_map.empty original_bindings
