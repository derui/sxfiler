open Sxfiler_core

module Contexts = struct
  type t = string list [@@deriving show, eq]
end
[@warning "-32"]

type contexts = Contexts.t

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

module Value = struct
  type t =
    { value : string
    ; contexts : Contexts.t }
  [@@deriving show, eq]
end
[@warning "-32"]

type t = {keymap : Value.t list Binding_map.t} [@@deriving show, eq]

let make () = {keymap = Binding_map.empty}

let update t ~contexts ~key ~value =
  Binding_map.update key
    (fun v ->
      let map_value = {Value.value; contexts} in
      let values = Option.get ~default:(fun () -> []) v |> List.filter (fun v -> v <> map_value) in
      Some (map_value :: values))
    t

let add t ~contexts ~key ~value =
  let key = Sxfiler_kbd.to_keyseq key in
  {keymap = update t.keymap ~contexts ~key ~value}

let remove t ~contexts ~key =
  let key = Sxfiler_kbd.to_keyseq key in
  let keymap =
    Binding_map.update key
      (fun v ->
        let v =
          Option.get ~default:(fun () -> []) v
          |> List.filter (fun v -> not @@ Contexts.equal v.Value.contexts contexts)
        in
        Some v)
      t.keymap
  in
  {keymap}

let bindings t =
  Binding_map.bindings t.keymap
  |> List.map (fun (key, values) ->
         let open Option in
         Sxfiler_kbd.of_keyseq key
         >|= fun kbd -> List.map (fun value -> (value.Value.contexts, kbd, value.value)) values)
  |> List.map (Option.get ~default:(fun () -> []))
  |> List.flatten
