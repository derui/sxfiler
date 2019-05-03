module D = Sxfiler_domain.Key_map

type key =
  { key : string
  ; action : string
  ; when_ : Condition.t [@key "when"] }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

type t = {bindings : key list} [@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

(* translator between domain and response/request type *)
let of_domain t =
  let bindings = D.bindings t in
  List.map
    (fun (cond, key, value) ->
       {when_ = Condition.of_domain cond; key = Sxfiler_kbd.to_keyseq key; action = value} )
    bindings
  |> fun v -> {bindings = v}

let to_domain t =
  let empty = D.make () in
  List.fold_left
    (fun keymap binding ->
       match Sxfiler_kbd.of_keyseq binding.key with
       | None -> keymap
       | Some key ->
         D.add keymap ~condition:(Condition.to_domain binding.when_) ~key ~value:binding.action )
    empty t.bindings
