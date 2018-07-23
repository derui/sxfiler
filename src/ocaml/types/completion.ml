(** Completion defines value object to complete something.  *)

(** {!Candidate} defines type of result of completion. *)
module Candidate = struct
  type 'a t = {
    start: int ;
    length: int;
    value: 'a;
  }
end

(** Common_item is useful type to complete by RPC with unknown types. *)
module Common_item = struct
  type t = {
    id: string;
    value: string;
  }
end

(** Type of source that is used to source of completion. *)
type 'a source = 'a list

(** Result of completion. Type variable ['a] should equals with {!source} having it.*)
type 'a result = 'a Candidate.t array
