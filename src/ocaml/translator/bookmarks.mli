type error = Invalid_path of string [@@deriving eq, show]

include
  Core.Domain_translator
    with type t := Sxfiler_generated.Bookmark.BookmarkList.t
     and type domain := Sxfiler_domain.Bookmarks.t
     and type error := error
