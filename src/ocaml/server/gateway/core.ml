(** This signature defines the signature of base gateway module. *)
module type Gateway = sig
  type params [@@deriving of_protocol ~driver:(module Protocol_conv_json.Json)]
  type result [@@deriving to_protocol ~driver:(module Protocol_conv_json.Json)]

  val handle : params -> result Lwt.t
end
