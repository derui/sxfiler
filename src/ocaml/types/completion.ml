(** Completion defines value object to complete something.  *)

(** {!Source_class} defines type of source for completion.  *)
module Source_class = struct
  type t =
    | File
    | History
    | Simple
  [@@deriving enum,show]
end

(** Item is only type to complete by RPC with any value. *)
module Item = struct
  type t = {
    id: string;
    value: string;
  }
end

(** {!Candidate} defines type of result of completion. *)
module Candidate = struct
  type 'a base = {
    start: int ;
    length: int;
    value: 'a;
  }

  type t = Item.t base
end

(** Type of collection that is used to source of completion. *)
type collection = Item.t list

(** Result of completion. *)
type result = Candidate.t array
