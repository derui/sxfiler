(************************************************)
(*       AUTOGENERATED FILE - DO NOT EDIT!      *)
(************************************************)
(* Generated by: ocaml-protoc-plugin            *)
(* https://github.com/issuu/ocaml-protoc-plugin *)
(************************************************)
(* Source: filer.proto Syntax: proto3 Parameters: debug=false annot='[@@deriving eq, show, protocol
   ~driver:(module Protocol_conv_json.Json)]' opens=[] int64_as_int=true int32_as_int=true
   fixed_as_int=false singleton_record=true *)

open Ocaml_protoc_plugin.Runtime [@@warning "-33"]

(**/**)

module Imported'modules = struct
  module Types = Types
end

(**/**)

module rec Capability : sig
  val name' : unit -> string

  type t = {
    writable : bool;
    readable : bool;
    executable : bool;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.Capability"

  type t = {
    writable : bool;
    readable : bool;
    executable : bool;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { writable; readable; executable } = f' [] writable readable executable in
    let spec =
      Runtime'.Serialize.C.(
        basic (1, bool, proto3) ^:: basic (2, bool, proto3) ^:: basic (3, bool, proto3) ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions writable readable executable = { writable; readable; executable } in
    let spec =
      Runtime'.Deserialize.C.(
        basic (1, bool, proto3) ^:: basic (2, bool, proto3) ^:: basic (3, bool, proto3) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and Mode : sig
  val name' : unit -> string

  type t = {
    owner : Capability.t option;
    group : Capability.t option;
    others : Capability.t option;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.Mode"

  type t = {
    owner : Capability.t option;
    group : Capability.t option;
    others : Capability.t option;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { owner; group; others } = f' [] owner group others in
    let spec =
      Runtime'.Serialize.C.(
        basic_opt (1, message (fun t -> Capability.to_proto t))
        ^:: basic_opt (2, message (fun t -> Capability.to_proto t))
        ^:: basic_opt (3, message (fun t -> Capability.to_proto t))
        ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions owner group others = { owner; group; others } in
    let spec =
      Runtime'.Deserialize.C.(
        basic_opt (1, message (fun t -> Capability.from_proto t))
        ^:: basic_opt (2, message (fun t -> Capability.from_proto t))
        ^:: basic_opt (3, message (fun t -> Capability.from_proto t))
        ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FileStat : sig
  val name' : unit -> string

  type t = {
    mode : Mode.t option;
    uid : int;
    gid : int;
    atime : string;
    ctime : string;
    mtime : string;
    size : string;
    isDirectory : bool;
    isFile : bool;
    isSymlink : bool;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FileStat"

  type t = {
    mode : Mode.t option;
    uid : int;
    gid : int;
    atime : string;
    ctime : string;
    mtime : string;
    size : string;
    isDirectory : bool;
    isFile : bool;
    isSymlink : bool;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { mode; uid; gid; atime; ctime; mtime; size; isDirectory; isFile; isSymlink } =
      f' [] mode uid gid atime ctime mtime size isDirectory isFile isSymlink
    in
    let spec =
      Runtime'.Serialize.C.(
        basic_opt (1, message (fun t -> Mode.to_proto t))
        ^:: basic (2, int32_int, proto3)
        ^:: basic (3, int32_int, proto3)
        ^:: basic (4, string, proto3)
        ^:: basic (5, string, proto3)
        ^:: basic (6, string, proto3)
        ^:: basic (7, string, proto3)
        ^:: basic (8, bool, proto3)
        ^:: basic (9, bool, proto3)
        ^:: basic (10, bool, proto3)
        ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions mode uid gid atime ctime mtime size isDirectory isFile isSymlink =
      { mode; uid; gid; atime; ctime; mtime; size; isDirectory; isFile; isSymlink }
    in
    let spec =
      Runtime'.Deserialize.C.(
        basic_opt (1, message (fun t -> Mode.from_proto t))
        ^:: basic (2, int32_int, proto3)
        ^:: basic (3, int32_int, proto3)
        ^:: basic (4, string, proto3)
        ^:: basic (5, string, proto3)
        ^:: basic (6, string, proto3)
        ^:: basic (7, string, proto3)
        ^:: basic (8, bool, proto3)
        ^:: basic (9, bool, proto3)
        ^:: basic (10, bool, proto3)
        ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FileItem : sig
  val name' : unit -> string

  type t = {
    id : string;
    parent : string;
    name : string;
    fullPath : string;
    stat : FileStat.t option;
    hasLinkPath : bool;
    linkPath : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FileItem"

  type t = {
    id : string;
    parent : string;
    name : string;
    fullPath : string;
    stat : FileStat.t option;
    hasLinkPath : bool;
    linkPath : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { id; parent; name; fullPath; stat; hasLinkPath; linkPath } =
      f' [] id parent name fullPath stat hasLinkPath linkPath
    in
    let spec =
      Runtime'.Serialize.C.(
        basic (1, string, proto3)
        ^:: basic (2, string, proto3)
        ^:: basic (3, string, proto3)
        ^:: basic (4, string, proto3)
        ^:: basic_opt (5, message (fun t -> FileStat.to_proto t))
        ^:: basic (6, bool, proto3)
        ^:: basic (7, string, proto3)
        ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions id parent name fullPath stat hasLinkPath linkPath =
      { id; parent; name; fullPath; stat; hasLinkPath; linkPath }
    in
    let spec =
      Runtime'.Deserialize.C.(
        basic (1, string, proto3)
        ^:: basic (2, string, proto3)
        ^:: basic (3, string, proto3)
        ^:: basic (4, string, proto3)
        ^:: basic_opt (5, message (fun t -> FileStat.from_proto t))
        ^:: basic (6, bool, proto3)
        ^:: basic (7, string, proto3)
        ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FileList : sig
  val name' : unit -> string

  type t = {
    location : string;
    items : FileItem.t list;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FileList"

  type t = {
    location : string;
    items : FileItem.t list;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { location; items } = f' [] location items in
    let spec =
      Runtime'.Serialize.C.(
        basic (1, string, proto3)
        ^:: repeated (2, message (fun t -> FileItem.to_proto t), not_packed)
        ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions location items = { location; items } in
    let spec =
      Runtime'.Deserialize.C.(
        basic (1, string, proto3)
        ^:: repeated (2, message (fun t -> FileItem.from_proto t), not_packed)
        ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and LocationHistory : sig
  val name' : unit -> string

  type t = {
    records : LocationRecord.t list;
    maxRecordNumber : int;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.LocationHistory"

  type t = {
    records : LocationRecord.t list;
    maxRecordNumber : int;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { records; maxRecordNumber } = f' [] records maxRecordNumber in
    let spec =
      Runtime'.Serialize.C.(
        repeated (1, message (fun t -> LocationRecord.to_proto t), not_packed)
        ^:: basic (2, int32_int, proto3)
        ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions records maxRecordNumber = { records; maxRecordNumber } in
    let spec =
      Runtime'.Deserialize.C.(
        repeated (1, message (fun t -> LocationRecord.from_proto t), not_packed)
        ^:: basic (2, int32_int, proto3)
        ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and LocationRecord : sig
  val name' : unit -> string

  type t = {
    location : string;
    timestamp : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.LocationRecord"

  type t = {
    location : string;
    timestamp : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { location; timestamp } = f' [] location timestamp in
    let spec =
      Runtime'.Serialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions location timestamp = { location; timestamp } in
    let spec =
      Runtime'.Deserialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and Filer : sig
  val name' : unit -> string

  type t = {
    id : string;
    name : string;
    fileList : FileList.t option;
    history : LocationHistory.t option;
    markedItems : string list;
    sortOrder : Imported'modules.Types.SortType.t;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.Filer"

  type t = {
    id : string;
    name : string;
    fileList : FileList.t option;
    history : LocationHistory.t option;
    markedItems : string list;
    sortOrder : Imported'modules.Types.SortType.t;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { id; name; fileList; history; markedItems; sortOrder } =
      f' [] id name fileList history markedItems sortOrder
    in
    let spec =
      Runtime'.Serialize.C.(
        basic (1, string, proto3)
        ^:: basic (2, string, proto3)
        ^:: basic_opt (3, message (fun t -> FileList.to_proto t))
        ^:: basic_opt (4, message (fun t -> LocationHistory.to_proto t))
        ^:: repeated (5, string, packed)
        ^:: basic (6, enum Imported'modules.Types.SortType.to_int, proto3)
        ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions id name fileList history markedItems sortOrder =
      { id; name; fileList; history; markedItems; sortOrder }
    in
    let spec =
      Runtime'.Deserialize.C.(
        basic (1, string, proto3)
        ^:: basic (2, string, proto3)
        ^:: basic_opt (3, message (fun t -> FileList.from_proto t))
        ^:: basic_opt (4, message (fun t -> LocationHistory.from_proto t))
        ^:: repeated (5, string, packed)
        ^:: basic (6, enum Imported'modules.Types.SortType.from_int, proto3)
        ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerMakeRequest : sig
  val name' : unit -> string

  type t = {
    initialLocation : string;
    name : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerMakeRequest"

  type t = {
    initialLocation : string;
    name : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { initialLocation; name } = f' [] initialLocation name in
    let spec =
      Runtime'.Serialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions initialLocation name = { initialLocation; name } in
    let spec =
      Runtime'.Deserialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerMakeResponse : sig
  val name' : unit -> string

  type t = { filer : Filer.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerMakeResponse"

  type t = { filer : Filer.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { filer } = f' [] filer in
    let spec = Runtime'.Serialize.C.(basic_opt (1, message (fun t -> Filer.to_proto t)) ^:: nil) in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions filer = { filer } in
    let spec =
      Runtime'.Deserialize.C.(basic_opt (1, message (fun t -> Filer.from_proto t)) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerGetRequest : sig
  val name' : unit -> string

  type t = { name : string }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerGetRequest"

  type t = { name : string }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { name } = f' [] name in
    let spec = Runtime'.Serialize.C.(basic (1, string, proto3) ^:: nil) in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions name = { name } in
    let spec = Runtime'.Deserialize.C.(basic (1, string, proto3) ^:: nil) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerGetResponse : sig
  val name' : unit -> string

  type t = { filer : Filer.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerGetResponse"

  type t = { filer : Filer.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { filer } = f' [] filer in
    let spec = Runtime'.Serialize.C.(basic_opt (1, message (fun t -> Filer.to_proto t)) ^:: nil) in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions filer = { filer } in
    let spec =
      Runtime'.Deserialize.C.(basic_opt (1, message (fun t -> Filer.from_proto t)) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerMoveParentRequest : sig
  val name' : unit -> string

  type t = { name : string }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerMoveParentRequest"

  type t = { name : string }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { name } = f' [] name in
    let spec = Runtime'.Serialize.C.(basic (1, string, proto3) ^:: nil) in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions name = { name } in
    let spec = Runtime'.Deserialize.C.(basic (1, string, proto3) ^:: nil) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerMoveParentResponse : sig
  val name' : unit -> string

  type t = { filer : Filer.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerMoveParentResponse"

  type t = { filer : Filer.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { filer } = f' [] filer in
    let spec = Runtime'.Serialize.C.(basic_opt (1, message (fun t -> Filer.to_proto t)) ^:: nil) in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions filer = { filer } in
    let spec =
      Runtime'.Deserialize.C.(basic_opt (1, message (fun t -> Filer.from_proto t)) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerEnterDirectoryRequest : sig
  val name' : unit -> string

  type t = {
    name : string;
    itemId : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerEnterDirectoryRequest"

  type t = {
    name : string;
    itemId : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { name; itemId } = f' [] name itemId in
    let spec =
      Runtime'.Serialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions name itemId = { name; itemId } in
    let spec =
      Runtime'.Deserialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerEnterDirectoryResponse : sig
  val name' : unit -> string

  type t = { filer : Filer.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerEnterDirectoryResponse"

  type t = { filer : Filer.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { filer } = f' [] filer in
    let spec = Runtime'.Serialize.C.(basic_opt (1, message (fun t -> Filer.to_proto t)) ^:: nil) in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions filer = { filer } in
    let spec =
      Runtime'.Deserialize.C.(basic_opt (1, message (fun t -> Filer.from_proto t)) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerToggleMarkRequest : sig
  val name' : unit -> string

  type t = {
    name : string;
    itemIds : string list;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerToggleMarkRequest"

  type t = {
    name : string;
    itemIds : string list;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { name; itemIds } = f' [] name itemIds in
    let spec =
      Runtime'.Serialize.C.(basic (1, string, proto3) ^:: repeated (2, string, packed) ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions name itemIds = { name; itemIds } in
    let spec =
      Runtime'.Deserialize.C.(basic (1, string, proto3) ^:: repeated (2, string, packed) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerToggleMarkResponse : sig
  val name' : unit -> string

  type t = { filer : Filer.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerToggleMarkResponse"

  type t = { filer : Filer.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { filer } = f' [] filer in
    let spec = Runtime'.Serialize.C.(basic_opt (1, message (fun t -> Filer.to_proto t)) ^:: nil) in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions filer = { filer } in
    let spec =
      Runtime'.Deserialize.C.(basic_opt (1, message (fun t -> Filer.from_proto t)) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerMoveRequest : sig
  val name' : unit -> string

  type t = {
    source : string;
    dest : string;
    itemIds : string list;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerMoveRequest"

  type t = {
    source : string;
    dest : string;
    itemIds : string list;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { source; dest; itemIds } = f' [] source dest itemIds in
    let spec =
      Runtime'.Serialize.C.(
        basic (1, string, proto3)
        ^:: basic (2, string, proto3)
        ^:: repeated (3, string, packed)
        ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions source dest itemIds = { source; dest; itemIds } in
    let spec =
      Runtime'.Deserialize.C.(
        basic (1, string, proto3)
        ^:: basic (2, string, proto3)
        ^:: repeated (3, string, packed)
        ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerMoveResponse : sig
  val name' : unit -> string

  type t = {
    taskId : string;
    taskName : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerMoveResponse"

  type t = {
    taskId : string;
    taskName : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { taskId; taskName } = f' [] taskId taskName in
    let spec =
      Runtime'.Serialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions taskId taskName = { taskId; taskName } in
    let spec =
      Runtime'.Deserialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerDeleteRequest : sig
  val name' : unit -> string

  type t = {
    source : string;
    itemIds : string list;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerDeleteRequest"

  type t = {
    source : string;
    itemIds : string list;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { source; itemIds } = f' [] source itemIds in
    let spec =
      Runtime'.Serialize.C.(basic (1, string, proto3) ^:: repeated (2, string, packed) ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions source itemIds = { source; itemIds } in
    let spec =
      Runtime'.Deserialize.C.(basic (1, string, proto3) ^:: repeated (2, string, packed) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerDeleteResponse : sig
  val name' : unit -> string

  type t = {
    taskId : string;
    taskName : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerDeleteResponse"

  type t = {
    taskId : string;
    taskName : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { taskId; taskName } = f' [] taskId taskName in
    let spec =
      Runtime'.Serialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions taskId taskName = { taskId; taskName } in
    let spec =
      Runtime'.Deserialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerCopyRequest : sig
  val name' : unit -> string

  type t = {
    source : string;
    dest : string;
    itemIds : string list;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerCopyRequest"

  type t = {
    source : string;
    dest : string;
    itemIds : string list;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { source; dest; itemIds } = f' [] source dest itemIds in
    let spec =
      Runtime'.Serialize.C.(
        basic (1, string, proto3)
        ^:: basic (2, string, proto3)
        ^:: repeated (3, string, packed)
        ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions source dest itemIds = { source; dest; itemIds } in
    let spec =
      Runtime'.Deserialize.C.(
        basic (1, string, proto3)
        ^:: basic (2, string, proto3)
        ^:: repeated (3, string, packed)
        ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerCopyResponse : sig
  val name' : unit -> string

  type t = {
    taskId : string;
    taskName : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerCopyResponse"

  type t = {
    taskId : string;
    taskName : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { taskId; taskName } = f' [] taskId taskName in
    let spec =
      Runtime'.Serialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions taskId taskName = { taskId; taskName } in
    let spec =
      Runtime'.Deserialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerJumpLocationRequest : sig
  val name' : unit -> string

  type t = {
    location : string;
    name : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerJumpLocationRequest"

  type t = {
    location : string;
    name : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { location; name } = f' [] location name in
    let spec =
      Runtime'.Serialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions location name = { location; name } in
    let spec =
      Runtime'.Deserialize.C.(basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end

and FilerJumpLocationResponse : sig
  val name' : unit -> string

  type t = { filer : Filer.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Runtime'.Writer.t
  val from_proto : Runtime'.Reader.t -> (t, [> Runtime'.Result.error ]) result
end = struct
  let name' () = "filer.FilerJumpLocationResponse"

  type t = { filer : Filer.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { filer } = f' [] filer in
    let spec = Runtime'.Serialize.C.(basic_opt (1, message (fun t -> Filer.to_proto t)) ^:: nil) in
    let serialize = Runtime'.Serialize.serialize [] spec in
    fun t -> apply ~f:serialize t

  let from_proto =
    let constructor _extensions filer = { filer } in
    let spec =
      Runtime'.Deserialize.C.(basic_opt (1, message (fun t -> Filer.from_proto t)) ^:: nil)
    in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
end