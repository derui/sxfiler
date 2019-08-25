open Sxfiler_core
module B = Sxfiler_domain.Bookmark

type t =
  { id : string
  ; path : string
  ; order : int }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

let to_domain t =
  match Uuidm.of_string t.id with
  | None -> failwith "Invalid identity format given"
  | Some id -> B.make ~id ~path:Path.(of_string t.path) ~order:t.order

let of_domain t = {id = Uuidm.to_string t.B.id; path = Path.to_string t.path; order = t.order}
