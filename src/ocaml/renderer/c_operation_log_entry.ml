module C = Sxfiler_common
module T = C.Types
module R = Jsoo_reactjs

module Log_type = struct
  module Component = R.Component.Make_stateless (struct
      class type t = object
        method logType: T.Operation_log.log_type Js.readonly_prop
      end
    end)

  let string_of_log_type = function
    | T.Operation_log.Info -> "[INFO ]"
    | T.Operation_log.Error -> "[ERROR]"

  let is_error = function
    | T.Operation_log.Error -> true
    | _ -> false

  let component = Component.make (fun props ->
      let class_name = Some Classnames.(let open Infix in
                                        return "fp-OperationLogEntry_LogType"
                                        <|> ( "fp-OperationLogEntry_LogType-Error", is_error props##.logType)
                                        |> to_string) in

      R.Dom.of_tag `span
        ~props:R.Core.Element_spec.({empty with class_name})
        ~children:[| R.text @@ string_of_log_type props##.logType |]
    )
end

module Component = R.Component.Make_stateless (struct
    class type t = object
      method entry: T.Operation_log.Entry.t Js.readonly_prop
    end
  end)

let component = Component.make (fun props ->
    let entry = props##.entry in
    let module E = T.Operation_log.Entry in
    let class_name = Some Classnames.(let open Infix in return "fp-OperationLogEntry" |> to_string) in

    R.Dom.of_tag `div
      ~props:R.Core.Element_spec.({empty with class_name})
      ~children:[|
        R.create_element ~key:"logType" ~props:(object%js
          val logType = entry.E.log_type
        end) Log_type.component;
        R.text entry.E.content;
      |]
  )
