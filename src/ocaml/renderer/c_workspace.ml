(** Layout container for viewer stack. *)
module T = Sxfiler_domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store


let file_list_container ~key store =
  let scanner = S.Scanner.Store.get @@ S.App.State.scanner store in

  [%c P_file_list_viewer.t ~key ~props:(object%js
      val scannerState = scanner
      val focused = true
    end)
  ]

let t = R.Component.make_stateless
    ~props:(module struct
             class type t = object
               method locator: (module Locator.S) Js.readonly_prop
             end
           end)
    ~render:(fun props ->
        let module L = (val props##.locator : Locator.S) in
        let store = S.App.Store.get L.store in
        let module C = T.Configuration in
        let class_name = Classnames.to_string ["fp-Workspace", true;] in

        [%e div ~key:"layout" ~class_name [
            file_list_container ~key:"scanner" store;
          ]]
      )
