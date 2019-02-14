(** the type that is JSON friendly for {!Sxfiler_domain.Filer.t} *)
type t =
  { id : string
  ; file_tree : File_tree.t
  ; history : Location_history.t
  ; selected_nodes : string list
  ; sort_order : Types.Sort_type.t }
[@@deriving yojson]

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Filer.t
