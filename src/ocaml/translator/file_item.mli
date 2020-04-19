type error =
  | No_stat      of string
  | Invalid_stat of File_stat.error
  | Invalid_path of string
[@@deriving eq, show]

include
  Core.Domain_translator
    with type t := Sxfiler_generated.Filer.FileItem.t
     and type domain := Sxfiler_domain.File_item.t
     and type error := error
