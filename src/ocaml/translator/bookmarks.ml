open Sxfiler_core
module B = Sxfiler_domain.Bookmarks
module G = Sxfiler_generated

type error = Invalid_path of string [@@deriving eq, show]

type t = G.Bookmark.BookmarkList.t

let convert_item (item : G.Bookmark.Bookmark.t) =
  let name = B.Name.make item.name and path = Path.of_string item.path in
  path
  |> Result.map (fun path -> (name, path))
  |> Result.map_error (function Path.Empty_path -> Invalid_path item.name)

let to_domain (t : t) =
  List.map convert_item t.items
  |> List.fold_left
       (fun accum v ->
         match (v, accum) with
         | Ok (name, path), Ok accum -> Ok (B.insert ~name ~path accum)
         | Error e, _ | _, Error e   -> Error e)
       (Ok B.empty)

let of_domain (t : B.t) =
  let items =
    B.items t
    |> List.map (fun v -> { G.Bookmark.Bookmark.name = B.Name.value v.B.Item.name; path = Path.to_string v.path })
  in
  { G.Bookmark.BookmarkList.items }
