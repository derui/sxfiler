type t
(** Type of error. *)

exception Error of t
(** Exception of error. This exception is created by [to_exn] *)

val create : string -> t
(** [create message] create new object [t] *)

val tag : t -> string -> t
(** [tag tag error] create new object tagged to [error] with [tag]. *)

val to_string : t -> string
(** [to_string error] convert [error] to string. *)

val to_exn : t -> exn
(** [to_exn error] convert error to a exception that is {!Error} *)
