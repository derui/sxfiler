open Abbrev
open Sxfiler_core
module E = Endpoint_error

let validation_error field message = E.Validation_error.make ~field ~message

(* implementation to add theme *)
type add_theme = F.Theme.Add_theme.work_flow -> Endpoint.t

let add_theme : add_theme =
 fun flow ->
  Endpoint.with_request (G.Theme.AddRequest.from_proto, G.Theme.AddResponse.to_proto)
    ~f:(fun (input : G.Theme.AddRequest.t) ->
      let module NE = D.Common.Not_empty_string in
      let open Result.Infix in
      let input =
        let* name = NE.make input.name |> Option.to_result ~none:(validation_error "name" "name must not be empty") in
        let description = NE.make input.description in
        let* color_codes =
          match input.color_codes with
          | [] -> Error (validation_error "color_codes" "color_codes must have least one color code")
          | _  ->
              List.fold_left
                (fun codes code ->
                  let* name =
                    NE.make code.G.Theme.ColorCode.name
                    |> Option.to_result ~none:(validation_error "color_codes.name" "name of color must not be empty")
                  in
                  let* code =
                    NE.make code.G.Theme.ColorCode.hex_color
                    |> Option.to_result ~none:(validation_error "color_codes.hex_color" "hex_color must not be empty")
                  in
                  Result.fmap codes ~f:(fun v -> (name, code) :: v))
                (Ok []) input.color_codes
        in
        Ok { F.Theme.Add_theme.name; description; color_codes }
      in
      match input with
      | Error err -> Lwt.return_error (E.invalid_input [ err ])
      | Ok input  -> (
          let%lwt response = flow input in
          match response with
          | Error (F.Theme.Invalid_color_format _) -> E.Keymap_error.empty_context |> E.keymap |> Lwt.return_error
          | Error (F.Theme.Store_error (F.Common_step.Theme.Store_theme.Duplicate_name v)) ->
              E.Theme_error.duplicated v |> E.theme |> Lwt.return_error
          | Error e -> Printf.sprintf "Unknown error: %s" (F.Theme.show_error e) |> E.unknown |> Lwt.return_error
          | Ok theme ->
              let res = G.Theme.AddResponse.{ theme = theme |> Tr.Theme.of_domain |> Option.some } in
              Lwt.return_ok (res, []) ))

(* implementation to remove theme *)
type remove_theme = F.Theme.Remove_theme.work_flow -> Endpoint.t

let remove_theme : remove_theme =
 fun flow ->
  Endpoint.with_request (G.Theme.RemoveRequest.from_proto, G.Theme.RemoveResponse.to_proto) ~f:(fun { name } ->
      let open Result.Infix in
      let module NE = D.Common.Not_empty_string in
      let input =
        let* name = NE.make name |> Option.to_result ~none:(validation_error "name" "must not be empty") in
        Ok { F.Theme.Remove_theme.name }
      in
      match input with
      | Error e  -> Lwt.return_error (E.invalid_input [ e ])
      | Ok input -> (
          match%lwt flow input with
          | Error (F.Theme.Remove_error (F.Common_step.Theme.Remove_theme.Not_found_theme _)) ->
              E.Theme_error.not_found_theme name |> E.theme |> Lwt.return_error
          | Error e -> Printf.sprintf "Unknown error: %s" (F.Theme.show_error e) |> E.unknown |> Lwt.return_error
          | Ok () -> Lwt.return_ok ((), []) ))

(* implementation to list theme *)
type list = F.Theme.List_theme.work_flow -> Endpoint.t

let list : list =
 fun flow ->
  Endpoint.with_request (G.Theme.ListRequest.from_proto, G.Theme.ListResponse.to_proto) ~f:(fun () ->
      let%lwt themes = flow () in
      let res = { G.Theme.ListResponse.themes = List.map Tr.Theme.of_domain themes } in
      Lwt.return_ok (res, []))
