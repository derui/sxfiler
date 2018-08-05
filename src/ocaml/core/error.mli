
(** Type of error. *)
type t

(** Exception of error. This exception is created by [to_exn] *)
exception Error of t

(** [create message] create new object [t] *)
val create : string -> t

(** [tag tag error] create new object tagged to [error] with [tag]. *)
val tag : t -> string -> t

(** [to_string error] convert [error] to string.  *)
val to_string : t -> string

(** [to_exn error] convert error to a exception that is {!Error} *)
val to_exn : t -> exn
