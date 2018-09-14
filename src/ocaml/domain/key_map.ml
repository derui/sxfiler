open Sxfiler_core
include Key_map_intf

module Binding_map = Map.Make (struct
    type t = string

    let compare = Pervasives.compare
  end)

module Original_key_binding = struct
  type 'a t =
    { value : 'a
    ; condition : Condition.t }
end

type 'a t = {keymap : 'a Original_key_binding.t list Binding_map.t}

let make () = {keymap = Binding_map.empty}

let update t ~condition ~key ~value =
  Binding_map.update key
    (fun v ->
       let values = Option.get ~default:(fun () -> []) v in
       let value = {Original_key_binding.condition; value} in
       Some (value :: values) )
    t

let add t ~condition ~key ~value =
  let key = Sxfiler_kbd.to_keyseq key in
  {keymap = update t.keymap ~condition ~key ~value}

let find t ~condition ~key =
  let key = Sxfiler_kbd.to_keyseq key in
  let bindings = Option.get ~default:(fun () -> []) @@ Binding_map.find_opt key t.keymap in
  let matched =
    List.filter
      (fun v -> Condition.subset ~parts:v.Original_key_binding.condition ~current:condition)
      bindings
  in
  match matched with [] -> None | v :: _ -> Some v.Original_key_binding.value

let bindings t =
  Binding_map.bindings t.keymap
  |> List.map (fun (key, values) ->
      let open Original_key_binding in
      let open Option in
      Sxfiler_kbd.of_keyseq key
      >|= fun kbd -> List.map (fun value -> (value.condition, kbd, value.value)) values )
  |> List.map (Option.get ~default:(fun () -> []))
  |> List.flatten

let subset t ~condition =
  bindings t
  |> List.filter (fun (cond, _, _) -> Condition.subset ~parts:cond ~current:condition)
  |> List.fold_left
    (fun keymap (condition, key, value) -> add keymap ~condition ~key ~value)
    (make ())
