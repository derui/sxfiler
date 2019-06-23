(** the type that is JSON friendly for {!Sxfiler_domain.Filer.t} *)
type t =
  { id : string
  ; name : string
  ; file_list : File_list.t [@key "fileList"]
  ; history : Location_history.t
  ; marked_items : string list [@key "markedItems"]
  ; sort_order : Types.Sort_type.t [@key "sortOrder"] }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Filer.t
