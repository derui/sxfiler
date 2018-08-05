module D = Sxfiler_domain.Key_map

type key = {
  condition: Condition.t [@key "when"];
  key: string;
  action: string;
}
[@@deriving yojson]

type t = {
  bindings: key list;
} [@@deriving yojson]

let of_domain t =
  let bindings = D.bindings t in
  List.map (fun (cond, key, value) -> {condition = Condition.of_domain cond;
                                       key = Sxfiler_kbd.to_keyseq key;
                                       action = value;})
    bindings
  |> fun v -> {bindings = v}

let to_domain t =
  let empty = D.make () in
  List.fold_left (fun keymap binding ->
      match Sxfiler_kbd.of_keyseq binding.key with
      | None -> keymap
      | Some key ->
        D.add keymap ~condition:(Condition.to_domain binding.condition)
          ~key ~value:binding.action
    ) empty
    t.bindings
