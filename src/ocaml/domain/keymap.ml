open Sxfiler_core

module Binding = struct
  type t = {
    context : Context.t;
    key : Sxfiler_kbd.t;
  }
  [@@deriving eq, show, ord]

  let make ~context ~key = { context; key }
end

module Action = struct
  type t = Action of string [@@deriving eq, show]

  let make v = Action v

  let value (Action v) = v
end

module Binding_map = struct
  include Map.Make (struct
    type t = Binding.t

    let compare = Binding.compare
  end)

  let pp pp_v fmt t =
    let list = Fmt.list & Fmt.pair Binding.pp pp_v in
    let formatter = Fmt.box list in
    let entries = bindings t in
    formatter fmt entries
end

type t = { keymap : Action.t Binding_map.t } [@@deriving show, eq]

let empty = { keymap = Binding_map.empty }

let add ~binding ~action { keymap } = { keymap = Binding_map.add binding action keymap }

let remove ~binding { keymap } = { keymap = Binding_map.remove binding keymap }

let bindings t = Binding_map.bindings t.keymap

let find_by ~key { keymap } =
  Binding_map.bindings keymap
  |> List.filter (fun (binding, _) -> Sxfiler_kbd.equal key binding.Binding.key)
  |> List.map fst
