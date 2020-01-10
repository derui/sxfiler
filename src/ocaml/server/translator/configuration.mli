type t = Sxfiler_server_generated.Configuration.Configuration.t
[@@deriving show, protocol ~driver:(module Protocol_conv_json.Json)]
(** the type that is JSON friendly for {!Sxfiler_domain.Configuration.t} *)

include Core.Domain_translator with type t := t and type domain := Sxfiler_domain.Configuration.t
