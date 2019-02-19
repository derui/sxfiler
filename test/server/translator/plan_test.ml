module D = Sxfiler_domain
module Tr = Sxfiler_server_translator

let test_set =
  [ ( "can translate from domain"
    , `Quick
    , fun () ->
      let actual =
        Test_fixtures.Plan.fixture
          [D.Plan.Target_node.no_problem "id"; D.Plan.Target_node.need_fix "id2"]
      in
      let expected =
        { Tr.Plan.id = actual.id
        ; target_nodes =
            [ {Tr.Plan.Target_node.node_id = "id"; prediction = Tr.Plan.Prediction.No_problem}
            ; {Tr.Plan.Target_node.node_id = "id2"; prediction = Tr.Plan.Prediction.Need_fix} ]
        }
      in
      Alcotest.(check @@ of_pp Tr.Plan.pp) "domain" expected (Tr.Plan.of_domain actual) )
  ; ( "can translate to domain without executor"
    , `Quick
    , fun () ->
      let expected =
        Test_fixtures.Plan.fixture
          [D.Plan.Target_node.no_problem "id"; D.Plan.Target_node.need_fix "id2"]
      in
      let actual =
        { Tr.Plan.id = expected.id
        ; target_nodes =
            [ {Tr.Plan.Target_node.node_id = "id"; prediction = Tr.Plan.Prediction.No_problem}
            ; {Tr.Plan.Target_node.node_id = "id"; prediction = Tr.Plan.Prediction.Need_fix} ] }
      in
      let translated = Tr.Plan.to_domain actual in
      Alcotest.(check @@ string) "domain" expected.id translated.id )
  ; ( "can translate to/from yojson"
    , `Quick
    , fun () ->
      let data =
        Test_fixtures.Plan.fixture
          [D.Plan.Target_node.no_problem "id"; D.Plan.Target_node.need_fix "id2"]
        |> Tr.Plan.of_domain
      in
      Alcotest.(check @@ result (of_pp Tr.Plan.pp) string)
        "yojson" (Ok data)
        Tr.Plan.(to_yojson data |> of_yojson) ) ]
