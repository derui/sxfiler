open Sxfiler_core
module D = Sxfiler_domain
module C = Sxfiler_server_core
module I = Sxfiler_server_infra

let filer =
  D.Filer.make ~id:"foo" ~location:(Path.of_string "/var") ~nodes:[]
    ~sort_order:D.Types.Sort_type.Date ~history:(D.Location_history.make ())

let testcases =
  [ ( "create new instance each execution"
    , `Quick
    , fun () ->
      let fixed_random_state = Random.State.make [||] in
      let module State = struct
        let get () = fixed_random_state
      end in
      let module R = I.Workbench_factory.Make (State) in
      let data = R.make {source = filer; dest = filer; nodes = []} in
      let data' = R.make {source = filer; dest = filer; nodes = []} in
      Alcotest.(check @@ of_pp Fmt.nop) "not same id" false (Uuidm.equal data.id data'.id) ) ]

let () = Alcotest.run "workbench repository" [("operations", testcases)]
