(** Define translator for between domain object and gateway object  *)
include Core.Translator with type t = File_stat.t and type target = Sxfiler_domain.File_stat.t
