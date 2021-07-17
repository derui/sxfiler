(** {!Time} module define representation for micro-second based time.

    Limitations:

    - Only have micro-second resolution.
    - All offset [t] created from float is UTC.
    - Do not have any calendar function. *)

type t
(** [t] is the abstract time of this module. [t] can have range between [min] and [max]. *)

val equal : t -> t -> bool
(** [equal t1 t2] returns equivalent between [t1] and [t2]. *)

val compare : t -> t -> int
(** [compare t1 t2] returns comparison result. Return -1 when [t1] is earlier than [t2], return 1 when [t1] is later
    than [t2], or return 0 when [t1] and [t2] are same. *)

val min : t
(** [min] assigned minimum time that is able to represent time by [t]. This value is equivalent to
    [0000-01-01T00:00:00.000000Z] *)

val max : t
(** [min] assigned minimum time that is able to represent time by [t]. This value is equivalent to
    [9999-12-31T23:59:59.999999Z] *)

val of_float : float -> t option
(** [of_float ns] returns the time represented by [ns]. [ns] must be epoch time. This function returns None if result of
    [ns] is underflow of minimum or overflow of maximum. *)

val of_rfc3339 : string -> t option
(** [of_rfc3339 time] returns the time specified by [time]. *)

val to_int64 : t -> int64
(** [to_int64 t] returns time of [t] that has micro-second resolution. Representation returned from this function
    *always* has micro-second resolution, so it is [1_000_000_000] if [t] is converted from [epoch = 1] *)

val to_float : t -> float
(** [to_float t] returns time of [t] that has seconds in fraction of the result and micro-seconds in exponent of it. *)

val to_rfc3339 : t -> string
(** [to_rfc3339 t] returns string that shows RFC3339. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] prints pritty-print for [t] that the format is RFC3339 with UTC timezone of. *)
