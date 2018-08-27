(** All errors raised from use cases. A use case should have one by one algebraic type,
    and define polymorphic variant as detail of error. *)

(** Basic interface for Use case. If use case do not have any error,
    the type of error would be unit.
*)
module type Usecase = sig
  type input
  type output
  type error

  (** [execute input] runs use case with input.  *)
  val execute: input -> (output, error) result Lwt.t
end
