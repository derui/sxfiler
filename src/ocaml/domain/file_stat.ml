open Sxfiler_core

module Capability = struct
  type t = {
    writable : bool;
    readable : bool;
    executable : bool;
  }
  [@@deriving eq, show]

  let make ~writable ~readable ~executable = { writable; readable; executable }
end

module Mode = struct
  type t = {
    owner : Capability.t;
    group : Capability.t;
    others : Capability.t;
  }
  [@@deriving eq, show]

  let make ~owner ~group ~others = { owner; group; others }

  let empty =
    let capability = Capability.make ~writable:true ~readable:true ~executable:true in
    { owner = capability; group = capability; others = capability }
end

module Size = struct
  type t = Size of int64 [@@deriving eq, show]

  type _t = t

  let make v =
    let open Int64 in
    if v < 0L then Error "Size must be greater or equal 0" else Ok (Size v)

  let value (Size v) = v

  include Comparable.Make (struct
    type t = _t

    let compare (Size v1) (Size v2) = Int64.compare v1 v2
  end)
end

module Uid = struct
  type t = Uid of int [@@deriving eq, show]

  type _t = t

  let make v = if v < 0 then Error "uid must be greater or equal 0" else Ok (Uid v)

  let value (Uid v) = v

  include Comparable.Make (struct
    type t = _t

    let compare (Uid v1) (Uid v2) = Int.compare v1 v2
  end)
end

module Gid = struct
  type t = Gid of int [@@deriving eq, show]

  type _t = t

  let make v = if v < 0 then Error "gid must be greater or equal 0" else Ok (Gid v)

  let value (Gid v) = v

  include Comparable.Make (struct
    type t = _t

    let compare (Gid v1) (Gid v2) = Int.compare v1 v2
  end)
end

module Kind = struct
  type t =
    | File
    | Directory
    | Symlink   of Path.t
  [@@deriving eq, show]
end

module Stat = struct
  type t = {
    mode : Mode.t;
    uid : Uid.t;
    gid : Gid.t;
    atime : Time.t;
    ctime : Time.t;
    mtime : Time.t;
    size : Size.t;
  }
  [@@deriving eq, show]

  let make ~mode ~uid ~gid ~atime ~ctime ~mtime ~size = { mode; uid; gid; atime; ctime; mtime; size }
end

type t = {
  stat : Stat.t;
  kind : Kind.t;
}
[@@deriving eq, show]
(** [t] is the type of file_stat *)

let make_file stat = { stat; kind = Kind.File }

let make_directory stat = { stat; kind = Kind.Directory }

let make_symlink ~stat ~link_path = { stat; kind = Kind.Symlink link_path }
