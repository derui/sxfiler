open Abbrev
open Sxfiler_core
module L = Sxfiler_infrastructure.Logger

let to_global_event = List.map (fun e -> F.Configuration e)

(* query endpoints *)
type get = F.Common_step.Configuration.load -> Endpoint.t

let get : get =
 fun load ->
  Endpoint.with_request (G.Configuration.GetRequest.from_proto, G.Configuration.GetResponse.to_proto) ~f:(fun () ->
      let%lwt store = load () in
      Lwt.return_ok ({ G.Configuration.GetResponse.configurations = Tr.Configuration_store.of_domain store }, []))

type update = F.Configuration.Update.work_flow -> Endpoint.t

let update : update =
 fun wf ->
  Endpoint.with_request (G.Configuration.UpdateRequest.from_proto, G.Configuration.UpdateResponse.to_proto)
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
          let%lwt events = wf input in
          Lwt.return_ok ({ G.Configuration.UpdateResponse.key = res.key }, to_global_event events))
