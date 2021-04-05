open Abbrev
open Sxfiler_core
module L = Sxfiler_infrastructure.Logger

let to_global_event = List.map (fun e -> F.Configuration e)

(* query endpoints *)

let get deps request =
  let%lwt instance = S.fetch ~tag:(fun c -> `Step_configuration_instance c) |> S.provide deps |> S.run in
  let module I = (val instance : F.Common_step.Configuration.Instance) in
  Endpoint.with_request (G.Configuration.GetRequest.from_proto, G.Configuration.GetResponse.to_proto) request
    ~f:(fun () ->
      let%lwt store = I.load () in
      Lwt.return_ok ({ G.Configuration.GetResponse.configurations = Tr.Configuration_store.of_domain store }, []))

let update deps request =
  Endpoint.with_request (G.Configuration.UpdateRequest.from_proto, G.Configuration.UpdateResponse.to_proto) request
    ~f:(fun res ->
      let module E = Endpoint_error in
      let open Result.Infix in
      let input =
        let* key =
          D.Configuration_store.Key.from_list res.key
          |> Option.to_result ~none:(E.Validation_error.make ~field:"key" ~message:"Invalid key")
        in
        let* value =
          try Yojson.Basic.from_string res.json_value |> Result.ok
          with Yojson.Json_error _ ->
            E.Validation_error.make ~field:"json_value" ~message:"Invalid JSON format" |> Result.error
        in
        Ok { F.Configuration.Update.key; value }
      in
      match input with
      | Error e  -> Lwt.return_error (E.invalid_input [ e ])
      | Ok input ->
          let%lwt events = F.Configuration.update input |> S.provide deps |> S.run in
          Lwt.return_ok ({ G.Configuration.UpdateResponse.key = res.key }, to_global_event events))
