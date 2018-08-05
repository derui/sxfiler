open Sxfiler_core

module D = Sxfiler_domain
module S = Sxfiler_server
module C = Sxfiler_server_core
module G = Sxfiler_server_gateway
module Co = Sxfiler_completion
module T = Sxfiler_server_translator
module U = Sxfiler_usecase

let completion_tests = [
  Alcotest_lwt.test_case "can setup common source" `Quick (fun switch () ->

      let called = ref [] in
      let module Usecase : Co.Usecase.Setup = struct
        type input = {
          source: Co.Domain.collection;
        }

        type output = unit Lwt.t
        let execute {source} = called := source; Lwt.return_unit
      end in
      let module Setup = G.Completion.Setup(Usecase) in
      let expected = [
        {Co.Domain.Item.id = "1"; value = "foo"};
        {Co.Domain.Item.id = "2"; value = "foobar"};
        {Co.Domain.Item.id = "3"; value = "bar ball"};
      ] in
      let%lwt res = Setup.handle {source = List.map T.Completion.Item.of_domain expected} in
      Alcotest.(check @@ list @@ of_pp Fmt.nop) "called" expected !called;
      Lwt.return_unit
    );

  Alcotest_lwt.test_case "can complete from common source stored before" `Quick (fun switch () ->
      let expected = [
        {
          Co.Domain.Candidate.start = 0;
          length = 3;
          value = {Co.Domain.Item.id = "1"; value = "foo"}
        };
        {
          start = 0;
          length = 6;
          value = {id = "2"; value = "foobar"};
        }
      ] in

      let module Usecase : Co.Usecase.Read = struct
        type input = {
          input: string;
        }
        type output = Co.Domain.result Lwt.t

        let execute _ = Lwt.return expected
      end in

      let module Read = G.Completion.Read(Usecase) in
      let%lwt res = Read.handle {input = "foo"} in
      Alcotest.(check @@ list @@ of_pp Fmt.nop) "read"
        (List.map T.Completion.Candidate.of_domain expected)
        res;
      Lwt.return_unit
    );
]

let testcases = [
  "rpc procedure : completion", completion_tests;
]
