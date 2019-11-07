type t = {
  id : string;
  path : string;
  order : int;
}
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Bookmark.t
