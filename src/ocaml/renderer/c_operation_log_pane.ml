
module C = Sxfiler_common
module T = C.Types
module R = Jsoo_reactjs

module Component = R.Component.Make_stateless (struct
    class type t = object
      method operationLog: T.Operation_log.t Js.readonly_prop
    end
  end)

let component = Component.make (fun props ->
    let entries = (props##.operationLog).T.Operation_log.entries in
    let children = List.mapi (fun index entry ->
        R.create_element ~key:(string_of_int index) ~props:(object%js
          val entry = entry
        end) C_operation_log_entry.component
      ) entries |> Array.of_list in
    R.Dom.of_tag `div
      ~props:R.(element_spec ~class_name:"fp-OperationLogPane" ())
      ~children:[|
        R.Dom.of_tag `div ~props:R.(element_spec ~class_name:"fp-OperationLogPane_ScrollContainer" ())
          ~children
      |]
  )
