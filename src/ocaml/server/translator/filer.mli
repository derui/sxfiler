(** the type that is JSON friendly for {!Sxfiler_domain.Filer.t} *)
type t =
  { id : string
  ; file_tree : File_tree.t [@key "fileTree"]
  ; history : Location_history.t
  ; marked_nodes : string list [@key "markedNodes"]
  ; sort_order : Types.Sort_type.t [@key "sortOrder"] }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Filer.t
