module M = Sxfiler_common.Message

type t =
    Next_item
  | Prev_item

let of_string = function
  | "Next_item" -> Next_item
  | "Prev_item" -> Prev_item
  | _ -> failwith "Unknown action"

let to_message = function
  | Next_item -> M.select_next_item 1
  | Prev_item -> M.select_prev_item 1
