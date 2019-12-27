module type Type = sig
  type input [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]
  type output [@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

  val input_from_pb : Ocaml_protoc_plugin.Reader.t -> input Ocaml_protoc_plugin.Result.t
  (** [input_from_pb message] converts representation for Protocol buffers as string to input type. *)

  val output_to_pb : output -> Ocaml_protoc_plugin.Writer.t
  (** [output_to_pb message] converts [output] to representation for Protocol buffers as string. *)
end

(** This signature defines the signature of base gateway module. *)
module type Gateway = sig
  include Type

  val handle : input -> (output, Gateway_error.t) result Lwt.t
end
