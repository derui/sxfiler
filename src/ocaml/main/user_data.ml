module C = Sxfiler_common
module M = Modules
open Sexplib.Std

type t = {
  left_pane_history: C.Pane_history.History.t list;
  right_pane_history: C.Pane_history.History.t list;
}
[@@deriving sexp]

let user_data_path = M.electron##.app##getPath (Js.string "userData")

let load_user_data () = failwith "not implement"
