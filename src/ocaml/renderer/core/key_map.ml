open Sxfiler_core

module Original_key_binding = struct
  type t = {
    key: string;
    action: Callable_action.t;
    condition: Types.Condition.t;
  }

  class type js = object
    method key: Js.js_string Js.t Js.readonly_prop
    method action: Js.js_string Js.t Js.readonly_prop
    method _when: Types.Condition.js Js.t Js.optdef Js.readonly_prop
  end

  let to_js : t -> js Js.t = fun t -> object%js
    val key = Js.string t.key
    val action = Js.string @@ Callable_action.to_string t.action
    val _when = Js.Optdef.return @@ Types.Condition.to_js t.condition
  end

  let of_js : js Js.t -> t = fun js -> {
      key = Js.to_string js##.key;
      action = Callable_action.of_string @@ Js.to_string js##.action;
      condition = Js.Optdef.map js##._when Types.Condition.of_js
                  |> Js.Optdef.to_option
                  |> Option.get ~default:(fun () -> Types.Condition.empty);
    }
end

type t = Original_key_binding.t list

let empty = []

let add t ~condition ~key ~action = {Original_key_binding.condition; key; action} :: t

let find t ~condition =
  let module O = Original_key_binding in
  let list = List.filter (fun v -> v.O.condition = condition) t in
  List.fold_left (fun bindings binding ->
      Key_bindings.add bindings ~key:binding.O.key ~action:binding.O.action
    ) Key_bindings.empty list

let to_json : t -> < > Js.t = fun t ->
  List.map Original_key_binding.to_js t
  |> Array.of_list
  |> Js.array
  |> Js.Unsafe.coerce

let of_json : < > Js.t -> t = fun js ->
  let js = Js.Unsafe.coerce js in
  Array.to_list @@ Js.to_array @@ Js.array_map Original_key_binding.of_js js
