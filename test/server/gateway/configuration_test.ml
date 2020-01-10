module D = Sxfiler_domain
module G = Sxfiler_server_gateway
module T = Sxfiler_server_translator
module U = Sxfiler_usecase
module Gen = Sxfiler_server_generated.Configuration

let test_set =
  [
    Alcotest_lwt.test_case "get current configuration" `Quick (fun _ () ->
        let expected = D.Configuration.{ default_sort_order = D.Types.Sort_type.Date } in
        let module Usecase = struct
          type input = unit
          type output = D.Configuration.t
          type error = unit

          let execute () = Lwt.return @@ Ok expected
        end in
        let module Gateway = G.Configuration.Get.Make (Usecase) in
        let%lwt res = Gateway.handle () in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "current"
          (Ok { Gen.GetResponse.configuration = Some (T.Configuration.of_domain expected) })
          res;
        Lwt.return_unit);
  ]
