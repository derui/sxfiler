(** This module defines common and simple algebric types. *)

type cursor_pos = int

(** {!Sort_type} defines methods to sort in this software. *)
module Sort_type = struct
  type t =
    | Name
    | Size
    | Date

  let to_string = function
    | Name -> "Name"
    | Size -> "Size"
    | Date -> "Date"

  let of_string = function
    | "Name" -> Name
    | "Size" -> Size
    | "Date" -> Date
    | _ -> failwith "Unknown constructor for Sort_type"

end

type file_id = string

module Layout = struct
  type t = Side_by_side

  let to_string = function
    | Side_by_side -> "Side_by_side"

  let of_string = function
    | "Side_by_side" -> Side_by_side
    | _ -> failwith "Unknown constructor for Layout"
end
