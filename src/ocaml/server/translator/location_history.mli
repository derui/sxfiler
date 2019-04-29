(** the type that is JSON friendly for {!Sxfiler_domain.Location_history.t} *)
type t =
  { records : Location_record.t list
  ; max_record_num : int }
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]

include
  Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Location_history.t
