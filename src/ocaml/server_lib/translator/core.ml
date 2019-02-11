(**
   Base signature of translator. All translator aligns this signature.
*)
module type Translator = sig
  (** base type of translator  *)
  type t

  (** Target type of conversion each way *)
  type target

  val of_target : target -> (t, string) result
  (** [of_target target] convert [target] to type [t]. *)

  val to_target : t -> target
  (** [to_target t] convert [t] to [target]  *)
end
