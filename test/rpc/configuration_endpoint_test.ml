open Sxfiler_core
module D = Sxfiler_domain
module R = Sxfiler_rpc
module G = Sxfiler_generated
module Pb = Ocaml_protoc_plugin
module F = Sxfiler_workflow
module T = Sxfiler_translator
module K = D.Configuration_store.Key
module S = Sxfiler_dependency

let test_set =
  let get_mock conf =
    (module struct
      let save _ = failwith "save"

      let load () = Lwt.return conf
    end : F.Common_step.Configuration.Instance)
  in
  let value_t = Alcotest.testable G.Configuration.Configuration.pp G.Configuration.Configuration.equal in
  [
    Alcotest_lwt.test_case "get configuration from step" `Quick (fun _ () ->
        let request =
          { G.Service.Request.id = "id"; command = G.Service.Command.CONFIGURATION_GET; payload = Bytes.empty }
        in
        let actual =
          D.Configuration_store.empty
          |> D.Configuration_store.put ~key:(K.from_list [ "sample" ] |> Option.get) ~value:(`String "sample")
        in
        let expected = T.Configuration_store.of_domain actual in
        let%lwt res, events =
          R.Configuration_endpoint.get
            (function `Step_configuration_instance c -> S.Context.value (get_mock actual) c)
            request
        in
        let payload =
          res.payload |> Bytes.to_string |> Pb.Reader.create |> G.Configuration.GetResponse.from_proto |> Result.get_ok
        in
        Alcotest.(check string) "same id" "id" res.id;
        Alcotest.(check & list & of_pp F.pp_event) "events" [] events;
        Alcotest.(check & of_pp G.Service.Status.pp) "invalid" G.Service.Status.SUCCESS res.status;
        Alcotest.(check & list value_t) "configuration" expected payload.configurations;
        Lwt.return_unit);
  ]
