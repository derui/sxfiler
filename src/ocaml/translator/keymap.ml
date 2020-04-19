open Sxfiler_core
module D = Sxfiler_domain.Keymap
module Gen = Sxfiler_generated

type error =
  | Invalid_context
  | Invalid_key     of string
[@@deriving eq, show]

(* translator between domain and response/request type *)
let of_domain (t : D.t) =
  let bindings = D.bindings t in
  let bindings =
    List.map
      (fun (binding, action) ->
        {
          Gen.Keymap.Binding.contexts = Context.of_domain binding.D.Binding.context;
          key = Sxfiler_kbd.to_keyseq binding.D.Binding.key;
          action = D.Action.value action;
        })
      bindings
  in
  Gen.Keymap.Keymap.{ bindings }

let to_domain (t : Gen.Keymap.Keymap.t) =
  let empty = D.empty in
  let open Result.Infix in
  List.fold_left
    (fun keymap (binding : Gen.Keymap.Binding.t) ->
      let* context = Context.to_domain binding.contexts |> Result.map_error (fun _ -> Invalid_context) in
      let* key = Sxfiler_kbd.of_keyseq binding.key |> Option.to_result ~none:(Invalid_key binding.key) in
      let binding' = D.Binding.make ~context ~key in
      let action = D.Action.make binding.action in
      Result.fmap keymap ~f:(D.add ~binding:binding' ~action))
    (Ok empty) t.bindings
