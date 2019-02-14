(** Completer provides simple completion interface via string. *)

module type S = sig
  (** The type of completer. *)
  type t

  val read : t -> input:string -> collection:Completion.collection -> Completion.Candidate.t list
  (** Match [input] from [candidates] with method specified by [match_type].

      This function gives module to stringify a value of type [candidates],
      so this function can handle any type of candidate.
  *)
end

module type Instance = sig
  module Completer : S

  val this : Completer.t
end
