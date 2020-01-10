module type Type = sig
  type input [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  type output [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
end

(** This signature defines the signature of base gateway module. *)
module type Gateway = sig
  include Type

  val handle : input -> (output, Gateway_error.t) result Lwt.t
end
