(** This module provides types for this gateway interfaces.
    Types in this module contains only primitive type that domain represent to.
*)

module Completion = struct
  module Item = struct
    type t =
      { id : string
      ; value : string }
  end

  module Candidate = struct
    type t =
      { start : int
      ; length : int
      ; value : Item.t }
  end
end

module Condition = struct
  type t = string list

  let empty = []
end

module Configuration = struct
  module Sort_type = struct
    type t = int
  end

  type t = {default_sort_order : Sort_type.t}
end

module File_stat = struct
  type t =
    { mode : string
    ; uid : int
    ; gid : int
    ; atime : string
    ; ctime : string
    ; mtime : string
    ; size : string
    ; is_directory : bool
    ; is_file : bool
    ; is_symlink : bool }

  let empty =
    { mode = "0777"
    ; uid = 1000
    ; gid = 1000
    ; atime = "0"
    ; ctime = "0"
    ; mtime = "0"
    ; size = "0"
    ; is_directory = false
    ; is_file = true
    ; is_symlink = false }
end

module Key_map = struct
  type key =
    { condition : Condition.t
    ; key : string
    ; action : string }

  type t = {bindings : key list}

  let empty = {bindings = []}
end

module Location_record = struct
  type t =
    { location : string
    ; timestamp : string }
end

module Location_history = struct
  type t =
    { records : Location_record.t list
    ; max_records : int }

  let empty = {records = []; max_records = 0}
end

module Node = struct
  type t =
    { id : string
    ; name : string
    ; stat : File_stat.t
    ; parent_directory : string
    ; link_path : string option }

  let empty = {id = ""; name = ""; stat = File_stat.empty; parent_directory = ""; link_path = None}
end

module Filer = struct
  type t =
    { id : string
    ; location : string
    ; nodes : Node.t list
    ; history : Location_history.t }

  let empty = {id = ""; location = ""; nodes = []; history = Location_history.empty}
end

module Tree_snapshot = struct
  type t =
    { directory : string
    ; nodes : Node.t list }
end

module Plan = struct
  type node_plan =
    { operation : int
    ; node : Node.t }

  type t =
    { source : node_plan list
    ; dest : node_plan list }

  let empty = {source = []; dest = []}
end
