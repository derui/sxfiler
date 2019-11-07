type t = {
  location : string;
  timestamp : string;
}
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]
(** the type that is JSON friendly for {!Sxfiler_domain.Location_record.t} *)

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Location_record.t
