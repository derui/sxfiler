open Abbrev
open Sxfiler_core
module E = Endpoint_error

let validation_error field message = E.Validation_error.make ~field ~message

(* implementation to add theme *)
type get = F.Theme.Get_theme.work_flow -> Endpoint.t

let get : get =
 fun flow ->
  Endpoint.with_request (G.Theme.GetRequest.from_proto, G.Theme.GetResponse.to_proto) ~f:(fun () ->
      let module NE = D.Common.Not_empty_string in
      let%lwt theme = flow () in
      let res = G.Theme.GetResponse.{ theme = theme |> Tr.Theme.of_domain |> Option.some } in
      Lwt.return_ok (res, []))

(* implementation to list theme *)
type update = F.Theme.Update_theme.work_flow -> Endpoint.t

let update : update =
 fun flow ->
  Endpoint.with_request (G.Theme.UpdateRequest.from_proto, G.Theme.UpdateResponse.to_proto)
    ~f:(fun (input : G.Theme.UpdateRequest.t) ->
      let module NE = D.Common.Not_empty_string in
      let input =
        let open Result.Infix in
        let base_theme = NE.make input.base_theme in
        let* color_codes =
          match input.color_pairs with
          | [] -> Error (validation_error "color_codes" "color_codes must have least one color code")
          | _  ->
              List.fold_left
                (fun codes code ->
                  let* name =
                    NE.make code.G.Theme.ColorPair.name
                    |> Option.to_result ~none:(validation_error "color_codes.name" "name of color must not be empty")
                  in
                  let* code =
                    NE.make code.G.Theme.ColorPair.hex_color
                    |> Option.to_result ~none:(validation_error "color_codes.hex_color" "hex_color must not be empty")
                  in
                  Result.fmap codes ~f:(fun v -> (name, code) :: v))
                (Ok []) input.color_pairs
        in
        Ok { F.Theme.Update_theme.base_theme; color_codes }
      in
      match input with
      | Error e  -> E.invalid_input [ e ] |> Lwt.return_error
      | Ok input -> (
          let%lwt theme = flow input in
          match theme with
          | Error (F.Theme.Invalid_color_format _) -> E.Theme_error.invalid_color_format |> E.theme |> Lwt.return_error
          | Error (F.Theme.Store_error _)          -> E.Theme_error.store_error |> E.theme |> Lwt.return_error
          | Ok theme                               ->
              let res = G.Theme.UpdateResponse.{ theme = theme |> Tr.Theme.of_domain |> Option.some } in
              Lwt.return_ok (res, []) ))
