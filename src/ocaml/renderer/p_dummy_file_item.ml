(** This component provides dummy element to be able to measure size of item
    in list anytime.
*)

module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module L = Modules.Lodash

let t =
  R.Component.make_stateless
    ~props:
      ( module struct
        class type t =
          object
            method forwardedRef : R.React.ref_ Js.t Js.optdef Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        let file_mode =
          [%c
            P_file_mode.t ~key:"mode"
              ~props:
                (object%js
                  val mode = 0l

                  val className = Js.string "fp-FileItem_FileMode"
                end)]
        and timestamp =
          [%c
            P_file_timestamp.t ~key:"timestamp"
              ~props:
                (object%js
                  val timestamp = 0.0
                end)]
        and file_size =
          [%c
            P_file_size.t ~key:"size"
              ~props:
                (object%js
                  val size = 0L

                  val className = Js.string "fp-FileItem_FileSize"
                end)]
        and file_name =
          [%c
            P_file_name.t ~key:"name"
              ~props:
                (object%js
                  val name = "dummy"

                  val isDirectory = false

                  val isSymbolicLink = false

                  val className = Js.string "fp-FileItem_FileName"
                end)]
        in
        let class_name = Classnames.to_string [("fp-FileItem", true); ("fp-FileItem-dummy", true)] in
        [%e li ~_ref:props##.forwardedRef ~class_name [file_mode; timestamp; file_size; file_name]]
      )
