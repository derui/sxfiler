open Sxfiler_core

module Capability : sig
  type t = private {
    writable : bool;
    readable : bool;
    executable : bool;
  }
  [@@deriving eq, show]

  val make : writable:bool -> readable:bool -> executable:bool -> t
  (** [make ~writable ~readable ~executable] makes new instance of [t] *)
end

module Mode : sig
  type t = private {
    owner : Capability.t;
    group : Capability.t;
    others : Capability.t;
  }
  [@@deriving eq, show]

  val make : owner:Capability.t -> group:Capability.t -> others:Capability.t -> t
  (** [make ~owner ~group ~others] makes new instance of [t] *)

  val empty : t
  (** [empty] is the empty mode that has all capability for owner, group and others *)
end

module Size : sig
  type t = private Size of int64 [@@deriving eq, show]

  val make : int64 -> (t, string) result
  (** [make v] makes new instance of [t] *)

  val value : t -> int64
  (** [value t] return value of [t] *)

  include Comparable.S with type t := t
end

module Uid : sig
  type t = private Uid of int [@@deriving eq, show]

  val make : int -> (t, string) result

  val value : t -> int

  include Comparable.S with type t := t
end

module Gid : sig
  type t = private Gid of int [@@deriving eq, show]

  val make : int -> (t, string) result

  val value : t -> int

  include Comparable.S with type t := t
end

module Kind : sig
  type t =
    | File
    | Directory
    | Symlink   of Path.t
  [@@deriving eq, show]
end

module Stat : sig
  type t = private {
    mode : Mode.t;
    uid : Uid.t;
    gid : Gid.t;
    atime : Time.t;
    ctime : Time.t;
    mtime : Time.t;
    size : Size.t;
  }
  [@@deriving eq, show]
  (** Type of stat of file. Note: The value of *time (atime, ctime, mtime) fields has time resolution in term of
      milliseconds, not seconds. *)

  val make : mode:Mode.t -> uid:Uid.t -> gid:Gid.t -> atime:Time.t -> ctime:Time.t -> mtime:Time.t -> size:Size.t -> t
end

type t = private {
  stat : Stat.t;
  kind : Kind.t;
}
[@@deriving eq, show]
(** [t] is the type of file_stat *)

val make_file : Stat.t -> t
(** [make_file stat] creates a [File_stat] for [File] *)

val make_directory : Stat.t -> t
(** [make_file stat] creates a [File_stat] for [Directory] *)

val make_symlink : stat:Stat.t -> link_path:Path.t -> t
(** [make_symlink ~stat ~link_path] creates a [File_stat] for [Symlink] *)
