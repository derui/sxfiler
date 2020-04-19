(** Base signature of translator for domain and a type. *)
module type Domain_translator = sig
  type t
  (** base type of translator *)

  type domain
  (** Target type of conversion each way *)

  type error

  val of_domain : domain -> t
  (** [of_domain domain] get the instance {!t} from [domain] *)

  val to_domain : t -> (domain, error) result
  (** [to_domain t] get the instance in domain from [t] *)
end

(** Base signature of oneway translator for domain and a type. *)
module type Oneway_domain_translator = sig
  type t
  (** base type of translator *)

  type domain
  (** Target type of conversion each way *)

  val of_domain : domain -> t
  (** [of_domain domain] get the instance {!t} from [domain] *)
end
