(** Scanner module provides type to scan file tree. *)

open Sxfiler_core

type t = {
  id: string;
  location: Path.t;
  nodes: Node.t list;
  history: Location_history.t;
}

let make ~id ~location ~nodes ~history = {
  id;
  location;
  nodes;
  history;
}

let move_location t ~location ~nodes clock =
  let record = Location_record.record_of ~location clock in
  let history = Location_history.add_record t.history ~record in
  {t with location; nodes; history}

(** Signature for repository of scanner. *)
module type Repository = sig
  (** [resolve id] returns scanner instance if already exists. *)
  val resolve: string -> t option Lwt.t

  (** [store scanner] stores [t] to any place. *)
  val store: t -> unit Lwt.t
end
