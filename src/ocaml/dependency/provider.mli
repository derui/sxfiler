open Sxfiler_core

type (+'a, 'r) t

type void
(** unified result. *)

include Monad.S2 with type ('a, 'r) t := ('a, 'r) t

module Context : sig
  type value
  (** type of value of context, but provides this type from creating new contexts *)

  type 'a t
  (** base type of context. *)

  val value : 'a -> 'a t -> value
end

val run : ('a, void) t -> 'a Lwt.t
(** [run program] run the [program] and get the result. *)

val provide : ('r -> Context.value) -> ('a, 'r) t -> ('a, 'v) t
(** [provide provider v] provides dependencies from context. *)

val fetch : tag:('a Context.t -> 'r) -> ('a, 'r) t
(** [fetch ~tag] fetch the dependency tagged [tag] from provider.

    This function can be used like above:

    let* user = fetch ~tag:(fun ctx -> `User of ctx) in let id = User.id user in (* This line leads type of context like
    User.t Context.t *) return id *)

val return_lwt : 'a Lwt.t -> ('a, 'b) t

val return_ok : 'a -> (('a, 'e) result, 'b) t
(** [return_ok result] is a shortcut function *)

val return_error : 'e -> (('a, 'e) result, 'b) t
(** [return_error result] is a shortcut function *)
