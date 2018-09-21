open Mocha_of_ocaml
module L = Sxfiler_virtualized_list

let prepare () =
  let div = Dom_html.createDiv Dom_html.document in
  div##setAttribute (Js.string "id") (Js.string "js") ;
  match Dom_html.getElementById_opt "test" with
  | Some v -> ignore @@ Dom.removeChild Dom_html.document v
  | None ->
    () ;
    let body =
      let nl = Dom_html.document##getElementsByTagName (Js.string "body") in
      match nl##item 0 |> Js.Opt.to_option with
      | None -> failwith "Not found body tag"
      | Some v -> v
    in
    Dom.appendChild body div

let append_div parent ?text attrs =
  let div = Dom_html.createDiv Dom_html.document in
  List.iter (fun (key, v) -> div##setAttribute (Js.string key) (Js.string v)) attrs ;
  (match text with None -> () | Some text -> div##.textContent := Js.Opt.return @@ Js.string text) ;
  Dom.appendChild parent div ; div

let () =
  "Virtualized list"
  >::: [ ( "can get all items if list can show all items"
           >:: fun () ->
             prepare () ;
             let index = Dom_html.getElementById "js" in
             index##setAttribute (Js.string "style")
               (Js.string "height: 40px; margin: 0px; padding: 0px") ;
             let style = "height: 10px;margin: 0px; padding: 0px" in
             append_div index ~text:"foo" [("id", "first"); ("style", style)] |> ignore ;
             append_div index ~text:"bar" [("id", "second"); ("height", style)] |> ignore ;
             append_div index ~text:"baz" [("id", "third"); ("height", style)] |> ignore ;
             let all_items = Array.init 10 succ in
             let lst =
               L.make ~item_height:10. () |> L.update_all_items all_items
               |> L.update_list_height (float_of_int index##.clientHeight)
               |> L.recalculate_visible_window 0
             in
             assert_strict_eq [|1; 2; 3; 4|] (L.get_items_in_window lst) )
       ; ( "can get partial items if list can not show all items"
           >:: fun () ->
             prepare () ;
             let index = Dom_html.getElementById "js" in
             index##setAttribute (Js.string "style")
               (Js.string "height: 20px; margin: 0px; padding: 0px") ;
             let style = "height: 10px;margin: 0px; padding: 0px" in
             append_div index ~text:"foo" [("id", "first"); ("style", style)] |> ignore ;
             append_div index ~text:"bar" [("id", "second"); ("height", style)] |> ignore ;
             append_div index ~text:"baz" [("id", "third"); ("height", style)] |> ignore ;
             let all_items = Array.init 3 succ in
             let lst =
               L.make ~item_height:10. () |> L.update_all_items all_items
               |> L.update_list_height (float_of_int index##.clientHeight)
               |> L.recalculate_visible_window 0
             in
             let new_lst = L.recalculate_visible_window 2 lst in
             let open Infix in
             assert_strict_eq [|1; 2|] @@ L.get_items_in_window lst
                                          <|> assert_strict_eq [|2; 3|] @@ L.get_items_in_window new_lst )
       ; ( "should be equal window between before and after move cursor"
           >:: fun () ->
             prepare () ;
             let index = Dom_html.getElementById "js" in
             index##setAttribute (Js.string "style")
               (Js.string "height: 20px; margin: 0px; padding: 0px") ;
             let style = "height: 10px;margin: 0px; padding: 0px" in
             append_div index ~text:"foo" [("id", "first"); ("style", style)] |> ignore ;
             append_div index ~text:"bar" [("id", "second"); ("height", style)] |> ignore ;
             append_div index ~text:"baz" [("id", "third"); ("height", style)] |> ignore ;
             let all_items = Array.init 3 succ in
             let lst =
               L.make ~item_height:10. () |> L.update_all_items all_items
               |> L.update_list_height (float_of_int index##.clientHeight)
               |> L.recalculate_visible_window 0
             in
             let step_one = L.recalculate_visible_window 1 lst in
             let step_two = L.recalculate_visible_window 0 lst in
             let open Infix in
             assert_strict_eq [|1; 2|] @@ L.get_items_in_window lst
                                          <|> assert_strict_eq [|1; 2|] @@ L.get_items_in_window step_one
                                                                           <|> assert_strict_eq [|1; 2|] @@ L.get_items_in_window step_two )
       ; ( "should be able to scroll window"
           >:: fun () ->
             prepare () ;
             let index = Dom_html.getElementById "js" in
             index##setAttribute (Js.string "style")
               (Js.string "height: 20px; margin: 0px; padding: 0px") ;
             let style = "height: 10px;margin: 0px; padding: 0px" in
             append_div index ~text:"foo" [("id", "first"); ("style", style)] |> ignore ;
             let all_items = Array.init 10 succ in
             let lst =
               L.make ~item_height:10. () |> L.update_all_items all_items
               |> L.update_list_height (float_of_int index##.clientHeight)
               |> L.recalculate_visible_window 0
             in
             let step_one = L.recalculate_visible_window 1 lst in
             let step_two = L.recalculate_visible_window 3 lst in
             let open Infix in
             assert_strict_eq [|1; 2|] @@ L.get_items_in_window lst
                                          <|> assert_strict_eq [|1; 2|] @@ L.get_items_in_window step_one
                                                                           <|> assert_strict_eq [|3; 4|] @@ L.get_items_in_window step_two )
       ; ( "should correct visible size if only visible size change"
           >:: fun () ->
             prepare () ;
             let all_items = Array.init 10 succ in
             let lst =
               L.make ~item_height:10. () |> L.update_all_items all_items |> L.update_list_height 40.
               |> L.recalculate_visible_window 7
             in
             let next = L.update_list_height 80. lst |> L.recalculate_visible_window 7 in
             let open Infix in
             assert_strict_eq [|7; 8; 9; 10|] @@ L.get_items_in_window lst
                                                 <|> assert_strict_eq [|3; 4; 5; 6; 7; 8; 9; 10|] @@ L.get_items_in_window next ) ]
