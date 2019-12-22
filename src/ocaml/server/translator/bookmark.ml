open Sxfiler_core
module B = Sxfiler_domain.Bookmark
module G = Sxfiler_server_generated

type t = G.Bookmark.Bookmark.t

let to_domain (t : t) =
  match Uuidm.of_string t.id with
  | None -> failwith "Invalid identity format given"
  | Some id -> B.make ~id ~path:Path.(of_string t.path) ~order:t.order

let of_domain (t : B.t) =
  { G.Bookmark.Bookmark.id = Uuidm.to_string t.B.id; path = Path.to_string t.path; order = t.order }
