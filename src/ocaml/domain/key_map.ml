open Sxfiler_core

module Binding_map = struct
  include Map.Make (struct
      type t = string

      let compare = Pervasives.compare
    end)

  let pp pp_v fmt t =
    let list = Fmt.list @@ Fmt.pair Fmt.string pp_v in
    let formatter = Fmt.box list in
    let entries = bindings t in
    formatter fmt entries
end

module Original_key_binding = struct
  type t =
    { value : string
    ; condition : Condition.t }
  [@@deriving show, eq]

  (* suppress warning to do not use 'show' *)
end
[@@warning "-32"]

type t = {keymap : Original_key_binding.t list Binding_map.t} [@@deriving show, eq]

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
