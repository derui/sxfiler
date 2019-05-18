module S = Sxfiler_server
module Jy = Jsonrpc_yojson

let test_set =
  [ Alcotest_lwt.test_case "parse request when request required by spec" `Quick (fun _ () ->
        let module Spec = struct
          module Gateway = struct
            type params = {foo : string}
            [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

            type result = int [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

            let _, f = Spy.wrap (fun (_ : params) -> Lwt.return 100)
            let handle params = f params
          end

          let method_ = "foo"
          let param_requirement = `Required
        end in
        let module Proc = S.Procedure.Make (Spec) in
        let%lwt res = Proc.handle (Some Spec.Gateway.(params_to_json {foo = "bar"})) in
        let module G = Sxfiler_server_gateway in
        Alcotest.(check @@ result (of_pp Fmt.nop) (of_pp Fmt.nop))
          "current" res
          (Ok (Some (Spec.Gateway.result_to_json 100))) ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "use default value when parameter not required" `Quick (fun _ () ->
        let module Spec = struct
          module Gateway = struct
            type params = {foo : string}
            [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

            type result = int [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

            let spy, f = Spy.wrap (fun (_ : params) -> Lwt.return 100)
            let handle params = f params
          end

          let method_ = "foo"
          let param_requirement = `Not_required {Gateway.foo = "boo"}
        end in
        let module Proc = S.Procedure.Make (Spec) in
        let%lwt _ = Proc.handle None in
        let module G = Sxfiler_server_gateway in
        Alcotest.(check @@ of_pp Fmt.nop)
          "current"
          Spy.Wrap.(called_args Spec.Gateway.spy)
          [{Spec.Gateway.foo = "boo"}] ;
        Lwt.return_unit ) ]
