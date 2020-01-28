open Sxfiler_core

(** {!Record} is the line of history. *)
module Record : sig
  type t = private {
    location : Path.t;
    timestamp : Time.t;
  }
  [@@deriving eq, show]

  val make : location:Path.t -> timestamp:Time.t -> t
  (** [make ~location ~timestamp] factory function for [t] *)
end

type t = private {
  records : Record.t list;
  max_record_num : Common.Positive_number.t;
}
[@@deriving eq, show]
(** [t] The type for histories for locations *)

val make : max_record_num:Common.Positive_number.t -> ?records:Record.t list -> unit -> t
(** [make ?max_record_num ()] gets new history.

    Use default value of [max_record_num] is [100] if it did not give any value. *)

val add_record : Record.t -> t -> t
(** [add_record record t] add new record into [t] *)
