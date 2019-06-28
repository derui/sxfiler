(** Base signature of translator for domain and a type. *)
module type Domain_translator = sig
  type t
  (** base type of translator *)

  type domain
  (** Target type of conversion each way *)

  val of_domain : domain -> t
  (** [of_domain domain] get the instance {!t} from [domain] *)

  val to_domain : t -> domain
  (** [to_domain t] get the instance {!Sxfiler_domain.File_stat.t} from [t] *)
end
