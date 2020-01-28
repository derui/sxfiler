open Sxfiler_core
module D = Sxfiler_domain

type error =
  | Empty_context
  | Invalid_key    of string
  | Invalid_keymap of Common_step_keymap.load_error
[@@deriving eq, show]

type event =
  | Added    of D.Keymap.t
  | Removed  of D.Keymap.t
  | Reloaded of D.Keymap.t
[@@deriving eq, show]

module Add_key_binding = struct
  type input = {
    context : D.Common.Not_empty_string.t list;
    key : D.Common.Not_empty_string.t;
    action : D.Common.Not_empty_string.t;
  }

  type work_flow = input -> (event list, error) result Lwt.t
  (** workflow to add a key binding for action to key map *)
end

module Remove_key_binding = struct
  type input = {
    context : D.Common.Not_empty_string.t list;
    key : D.Common.Not_empty_string.t;
  }

  type work_flow = input -> (event list, error) result Lwt.t
  (** workflow to remove a key binding for action to key map *)
end

module Reload = struct
  type input = { path : Path.t }

  type work_flow = input -> (event list, error) result Lwt.t
end

type commands =
  | Add_key_binding    of Add_key_binding.input
  | Remove_key_binding of Remove_key_binding.input

let add_key_binding resolve_keymap store_keymap : Add_key_binding.work_flow =
 fun { context; key; action } ->
  let%lwt keymap = resolve_keymap () in
  let open Result.Infix in
  let keymap =
    let value = D.Common.Not_empty_string.value in
    let* key = value key |> Sxfiler_kbd.of_keyseq |> Option.to_result ~none:(Invalid_key (value key)) in
    let context = List.map value context |> D.Context.of_list and action = value action |> D.Keymap.Action.make in
    let binding = D.Keymap.Binding.make ~context ~key in
    D.Keymap.add ~binding ~action keymap |> Result.ok
  in
  match keymap with
  | Error e   -> Lwt.return_error e
  | Ok keymap ->
      let%lwt () = store_keymap keymap in
      [ Added keymap ] |> Lwt.return_ok

let remove_key_binding resolve_keymap store_keymap : Remove_key_binding.work_flow =
 fun { key; context } ->
  let%lwt keymap = resolve_keymap () in
  let open Result.Infix in
  let keymap =
    let value = D.Common.Not_empty_string.value in
    let* key = value key |> Sxfiler_kbd.of_keyseq |> Option.to_result ~none:(Invalid_key (value key)) in
    let context = List.map value context |> D.Context.of_list in
    let binding = D.Keymap.Binding.make ~context ~key in
    D.Keymap.remove ~binding keymap |> Result.ok
  in
  Lwt_result.(
    lift keymap >>= fun keymap ->
    let%lwt () = store_keymap keymap in
    [ Removed keymap ] |> Lwt.return_ok)

let reload load_keymap store_keymap : Reload.work_flow =
 fun { path } ->
  let%lwt keymap = load_keymap path in
  match keymap with
  | Ok keymap ->
      let%lwt () = store_keymap keymap in
      [ Reloaded keymap ] |> Lwt.return_ok
  | Error e   -> Lwt.return_error (Invalid_keymap e)
