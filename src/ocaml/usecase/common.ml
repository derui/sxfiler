(** All errors raised from use cases. A use case should have one by one algebraic type,
    and define polymorphic variant as detail of error. *)
type error =
  | GetFilerError of [`Not_found]
  | MakeFilerError of [`Already_exists]
  | No_error

(** Basic interface for Use case. *)
module type Usecase = sig
  type input
  type output

  (** [execute input] runs use case with input.  *)
  val execute: input -> (output, error) result Lwt.t
end
