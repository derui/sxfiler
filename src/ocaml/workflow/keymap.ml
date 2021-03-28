open Sxfiler_core
open Abbrev
include Keymap_intf

let add_key_binding { Add_key_binding.context; key; action } =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_keymap_instance c) in
  let module I = (val instance : Common_step_keymap.Instance) in
  let* keymap = I.resolve_keymap () |> S.return_lwt in
  let keymap =
    let open Result.Infix in
    let value = D.Common.Not_empty_string.value in
    let* key = value key |> Sxfiler_kbd.of_keyseq |> Option.to_result ~none:(Invalid_key (value key)) in
    let context = List.map value context |> D.Context.of_list and action = value action |> D.Keymap.Action.make in
    let binding = D.Keymap.Binding.make ~context ~key in
    D.Keymap.add ~binding ~action keymap |> Result.ok
  in
  match keymap with
  | Error e   -> S.return_error e
  | Ok keymap ->
      let* () = I.store_keymap keymap |> S.return_lwt in
      S.return_ok [ Added keymap ]

let remove_key_binding { Remove_key_binding.key; context } =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_keymap_instance c) in
  let module I = (val instance : Common_step_keymap.Instance) in
  let* keymap = I.resolve_keymap () |> S.return_lwt in
  let keymap =
    let open Result.Infix in
    let value = D.Common.Not_empty_string.value in
    let* key = value key |> Sxfiler_kbd.of_keyseq |> Option.to_result ~none:(Invalid_key (value key)) in
    let context = List.map value context |> D.Context.of_list in
    let binding = D.Keymap.Binding.make ~context ~key in
    D.Keymap.remove ~binding keymap |> Result.ok
  in
  match keymap with
  | Error e   -> S.return_error e
  | Ok keymap ->
      let* () = I.store_keymap keymap |> S.return_lwt in
      [ Removed keymap ] |> S.return_ok

let reload { Reload.path } =
  let open S.Infix in
  let* instance = S.fetch ~tag:(fun c -> `Step_keymap_instance c) in
  let module I = (val instance : Common_step_keymap.Instance) in
  let* keymap = I.load_keymap path |> S.return_lwt in
  match keymap with
  | Ok keymap ->
      let* () = I.store_keymap keymap |> S.return_lwt in
      [ Reloaded keymap ] |> S.return_ok
  | Error e   -> S.return_error (Invalid_keymap e)
