open Abbrev
open Sxfiler_core
module E = Endpoint_error

let to_global_events = List.map (fun v -> F.Keymap v)

(** endpoint to add key binding to current key map *)
let add_key_binding deps request =
  Endpoint.with_request (G.Keymap.AddKeyBindingRequest.from_proto, G.Keymap.AddKeyBindingResponse.to_proto) request
    ~f:(fun (input : G.Keymap.AddKeyBindingRequest.t) ->
      let open Result.Infix in
      let input =
        let* key =
          D.Common.Not_empty_string.make input.key
          |> Option.to_result ~none:(E.Validation_error.make ~field:"key" ~message:"empty key")
        in
        let* action =
          D.Common.Not_empty_string.make input.action
          |> Option.to_result ~none:(E.Validation_error.make ~field:"action" ~message:"empty action")
        in
        let* context =
          let context' = List.map D.Common.Not_empty_string.make input.contexts in
          if List.for_all Option.is_some context' then List.map Option.get context' |> Result.ok
          else Error (E.Validation_error.make ~field:"context" ~message:"empty context")
        in
        Ok { F.Keymap.Add_key_binding.key; context; action }
      in
      match input with
      | Error err -> Lwt.return_error (E.invalid_input [ err ])
      | Ok input  -> (
          let%lwt response = F.Keymap.add_key_binding input |> S.provide deps |> S.run in
          match response with
          | Error F.Keymap.Empty_context      -> E.Keymap_error.empty_context |> E.keymap |> Lwt.return_error
          | Error (F.Keymap.Invalid_key s)    -> E.Keymap_error.invalid_key s |> E.keymap |> Lwt.return_error
          | Error (F.Keymap.Invalid_keymap _) -> E.Keymap_error.invalid_keymap |> E.keymap |> Lwt.return_error
          | Ok events                         -> Lwt.return_ok ((), to_global_events events) ))

let remove_key_binding deps request =
  Endpoint.with_request (G.Keymap.AddKeyBindingRequest.from_proto, G.Keymap.AddKeyBindingResponse.to_proto) request
    ~f:(fun (input : G.Keymap.AddKeyBindingRequest.t) ->
      let open Result.Infix in
      let input =
        let* key =
          D.Common.Not_empty_string.make input.key
          |> Option.to_result ~none:(E.Validation_error.make ~field:"key" ~message:"empty key")
        in
        let* context =
          let context' = List.map D.Common.Not_empty_string.make input.contexts in
          if List.for_all Option.is_some context' then List.map Option.get context' |> Result.ok
          else Error (E.Validation_error.make ~field:"context" ~message:"empty context")
        in
        Ok { F.Keymap.Remove_key_binding.key; context }
      in
      match input with
      | Error err -> E.invalid_input [ err ] |> Lwt.return_error
      | Ok input  -> (
          let%lwt response = F.Keymap.remove_key_binding input |> S.provide deps |> S.run in
          match response with
          | Error F.Keymap.Empty_context      -> E.Keymap_error.empty_context |> E.keymap |> Lwt.return_error
          | Error (F.Keymap.Invalid_key s)    -> E.Keymap_error.invalid_key s |> E.keymap |> Lwt.return_error
          | Error (F.Keymap.Invalid_keymap _) -> E.Keymap_error.invalid_keymap |> E.keymap |> Lwt.return_error
          | Ok events                         -> Lwt.return_ok ((), to_global_events events) ))

let reload path deps request =
  Endpoint.with_request (G.Keymap.ReloadRequest.from_proto, G.Keymap.ReloadResponse.to_proto) request ~f:(fun () ->
      let input = { F.Keymap.Reload.path } in
      let%lwt response = F.Keymap.reload input |> S.provide deps |> S.run in
      match response with
      | Error F.Keymap.Empty_context      -> E.Keymap_error.empty_context |> E.keymap |> Lwt.return_error
      | Error (F.Keymap.Invalid_key s)    -> E.Keymap_error.invalid_key s |> E.keymap |> Lwt.return_error
      | Error (F.Keymap.Invalid_keymap _) -> E.Keymap_error.invalid_keymap |> E.keymap |> Lwt.return_error
      | Ok events                         -> Lwt.return_ok ((), to_global_events events))

(** Reload key map from specified path *)

(* query endpoints *)
let get deps request =
  Endpoint.with_request (G.Keymap.GetRequest.from_proto, G.Keymap.GetResponse.to_proto) request ~f:(fun () ->
      let%lwt instance = S.fetch ~tag:(fun c -> `Step_keymap_instance c) |> S.provide deps |> S.run in
      let module I = (val instance : F.Common_step.Keymap.Instance) in
      let%lwt keymap = I.resolve_keymap () in
      Lwt.return_ok ({ G.Keymap.GetResponse.keymap = Tr.Keymap.of_domain keymap |> Option.some }, []))
