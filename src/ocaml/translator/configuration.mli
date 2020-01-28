type t = Sxfiler_generated.Configuration.Configuration.t

type error =
  | Max_history_num    of int
  | Default_sort_order of string
[@@deriving eq, show]

include
  Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Configuration.t and type error := error
