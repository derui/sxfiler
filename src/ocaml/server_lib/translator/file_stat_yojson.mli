(** Define translator for between JSON and gateway object  *)
include Core.Translator with type t = File_stat.t and type target = Yojson.Safe.t
