module D = Sxfiler_domain
module G = Sxfiler_server_gateway
module Co = Sxfiler_domain.Completion
module T = Sxfiler_server_translator
module U = Sxfiler_usecase

let test_set =
  [ Alcotest_lwt.test_case "can setup common source" `Quick (fun _ () ->
        let called = ref [] in
        let module Usecase : U.Completion.Setup.S = struct
          include U.Completion.Setup.Type

          let execute {source} : (unit, unit) result Lwt.t =
            called := source ;
            Lwt.return_ok ()
        end in
        let module Setup = G.Completion.Setup.Make (Usecase) in
        let expected =
          [ {Co.Item.id = "1"; value = "foo"}
          ; {Co.Item.id = "2"; value = "foobar"}
          ; {Co.Item.id = "3"; value = "bar ball"} ]
        in
        let%lwt _ = Setup.handle {source = T.Completion.Collection.of_domain expected} in
        Alcotest.(check @@ list @@ of_pp Fmt.nop) "called" expected !called ;
        Lwt.return_unit )
  ; Alcotest_lwt.test_case "can complete from common source stored before" `Quick (fun _ () ->
        let expected =
          [ {Co.Candidate.start = 0; length = 3; value = {Co.Item.id = "1"; value = "foo"}}
          ; {start = 0; length = 6; value = {id = "2"; value = "foobar"}} ]
        in
        let module Usecase : U.Completion.Read.S = struct
          include U.Completion.Read.Type

          let execute _ = Lwt.return_ok expected
        end in
        let module Read = G.Completion.Read.Make (Usecase) in
        let%lwt res = Read.handle {input = "foo"} in
        Alcotest.(check @@ list @@ of_pp Fmt.nop)
          "read"
          (T.Completion.Candidates.of_domain expected)
          res ;
        Lwt.return_unit ) ]
