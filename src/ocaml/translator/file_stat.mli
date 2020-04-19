type error =
  | Invalid_capability
  | No_mode
  | Invalid_time       of string
  | Invalid_uid        of string
  | Invalid_gid        of string
  | Invalid_size       of string
  | Invalid_link_path  of string
[@@deriving eq, show]

include
  Core.Domain_translator
    with type t := Sxfiler_generated.Filer.FileStat.t
     and type domain := Sxfiler_domain.File_stat.t
     and type error := error
