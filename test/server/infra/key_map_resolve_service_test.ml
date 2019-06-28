open Sxfiler_core
module I = Sxfiler_server_infra
module D = Sxfiler_domain
module F = Test_fixtures

let key_map =
  let module K = Sxfiler_kbd in
  let bindings =
    [ ([], K.of_keyseq "k" |> Option.get_exn, "action1")
    ; (["foo"], K.of_keyseq "j" |> Option.get_exn, "action2")
    ; (["bar"], K.of_keyseq "h" |> Option.get_exn, "action3") ]
  in
  let key_map = D.Key_map.make () in
  List.fold_left
    (fun key_map (contexts, key, value) -> D.Key_map.add ~contexts ~key ~value key_map)
    key_map bindings

let test_set =
  [ Alcotest_lwt.test_case "resolve key map from path" `Quick (fun _ () ->
        let module Dummy = struct
          let getcwd () = Sys.getcwd ()
        end in
        let to_path s = Path.of_string s |> Path.resolve (module Dummy) in
        let path = to_path "./data_real/key_map/test.json" in
        let%lwt resolved = I.Key_map_resolve_service.resolve path in
        Alcotest.(check @@ F.Testable.key_map) "key_map" resolved key_map ;
        Lwt.return_unit) ]
