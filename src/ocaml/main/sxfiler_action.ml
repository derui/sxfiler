module M = Sxfiler_common.Message

type t =
    Next_item
  | Prev_item
[@@deriving yojson]

let to_message = function
  | Next_item -> M.select_next_item 1
  | Prev_item -> M.select_prev_item 1
