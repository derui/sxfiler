open Sxfiler_core
module S = Sxfiler_server
module C = Sxfiler_server_core
module G = Sxfiler_server_gateway
module Co = Sxfiler_domain.Completion
module Tr = Sxfiler_server_translator
module T = Sxfiler_rpc.Types
module Jy = Jsonrpc_ocaml_yojson

let test_set =
  [ Alcotest_lwt.test_case "can setup common source" `Quick (fun _ () ->
        let module State = C.Statable.Make (struct
            type t = T.Completion.Item.t list

            let empty () = []
          end) in
        let module Setup_gateway : G.Completion.Setup = struct
          type params = {source : T.Completion.Item.t list}

          let params_of_yojson js =
            let open Yojson.Safe.Util in
            let open Result in
            let source = js |> member "source" |> to_list in
            let source =
              List.fold_left
                (fun accum item ->
                   accum
                   >>= fun accum ->
                   Tr.Completion.Item.of_yojson item >>= fun item -> Ok (item :: accum) )
                (Ok []) source
            in
            source >>= fun source -> Ok {source = List.rev source}

          type result = unit

          let handle {source} = State.update source
        end in
        let proc = S.Proc_completion.setup_spec (module Setup_gateway) in
        let expected =
          [ {T.Completion.Item.id = "1"; value = "foo"}
          ; {T.Completion.Item.id = "2"; value = "foobar"}
          ; {T.Completion.Item.id = "3"; value = "bar ball"} ]
        in
        let id = Random.int64 Int64.max_int in
        let%lwt _ =
          proc.S.Procedure.handler
            Jy.Request.
              { id = Some id
              ; _method = ""
              ; params =
                  Some
                    (`Assoc [("source", `List (List.map Tr.Completion.Item.to_yojson expected))])
              }
        in
        let%lwt state = State.get () in
        Alcotest.(check @@ list @@ of_pp @@ Fmt.nop) "created" expected state ;
        Lwt.return_unit ) ]
