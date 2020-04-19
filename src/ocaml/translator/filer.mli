(** Translator for [Filer.t]. This type allows only OCaml type to protobuf type *)

include
  Core.Oneway_domain_translator with type t := Sxfiler_generated.Filer.Filer.t and type domain := Sxfiler_domain.Filer.t
