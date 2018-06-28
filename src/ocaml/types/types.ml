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

(** {!Layout} defines type to preset of layout *)
module Layout = struct
  type t = Side_by_side

  let to_string = function
    | Side_by_side -> "Side_by_side"

  let of_string = function
    | "Side_by_side" -> Side_by_side
    | _ -> failwith "Unknown constructor for Layout"
end

(** {!Source_type} defines type of source for completion.  *)
module Source_type = struct
  type t =
    | File
    | Directory_tree
    | History

  let to_string = function
    | File -> "File"
    | Directory_tree -> "Directory_tree"
    | History -> "History"

  let of_string = function
    | "File" -> File
    | "Directory_tree" -> Directory_tree
    | "History" -> History
    | _ -> failwith "Unknown constructor for Source_type"
end
