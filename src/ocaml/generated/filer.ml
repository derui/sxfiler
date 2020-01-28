(************************************************)
(*       AUTOGENERATED FILE - DO NOT EDIT!      *)
(************************************************)
(* Generated by: ocaml-protoc-plugin            *)
(* https://github.com/issuu/ocaml-protoc-plugin *)
(************************************************)
(*
   Source: filer.proto
   Syntax: proto3 
   Parameters:
     debug=false
     annot='[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]'
     opens=[]
     int64_as_int=true
     int32_as_int=true
     fixed_as_int=false
     singleton_record=true
*)

open Ocaml_protoc_plugin.Runtime [@@warning "-33"]
(**/**)
module Imported'modules = struct
  module Types = Types
end
(**/**)
module rec Side : sig
  type t = LEFT | RIGHT [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_int: t -> int
  val from_int: int -> (t, [> Runtime'.Result.error]) result
end = struct 
  type t = LEFT | RIGHT [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_int = function
    | LEFT -> 0
    | RIGHT -> 1
  
  let from_int = function
    | 0 -> Ok LEFT
    | 1 -> Ok RIGHT
    | n -> Error (`Unknown_enum_value n)
  
end
and Action : sig
  type t = RENAME | OVERWRITE | CANCEL [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_int: t -> int
  val from_int: int -> (t, [> Runtime'.Result.error]) result
end = struct 
  type t = RENAME | OVERWRITE | CANCEL [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_int = function
    | RENAME -> 0
    | OVERWRITE -> 1
    | CANCEL -> 2
  
  let from_int = function
    | 0 -> Ok RENAME
    | 1 -> Ok OVERWRITE
    | 2 -> Ok CANCEL
    | n -> Error (`Unknown_enum_value n)
  
end
and Capability : sig
  val name': unit -> string
  type t = { writable: bool; readable: bool; executable: bool } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.Capability"
  type t = { writable: bool; readable: bool; executable: bool }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { writable; readable; executable } -> f' [] writable readable executable in
    let spec = Runtime'.Serialize.C.( basic (1, bool, proto3) ^:: basic (2, bool, proto3) ^:: basic (3, bool, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions writable readable executable -> { writable; readable; executable } in
    let spec = Runtime'.Deserialize.C.( basic (1, bool, proto3) ^:: basic (2, bool, proto3) ^:: basic (3, bool, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and Mode : sig
  val name': unit -> string
  type t = { owner: Capability.t option; group: Capability.t option; others: Capability.t option } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.Mode"
  type t = { owner: Capability.t option; group: Capability.t option; others: Capability.t option }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { owner; group; others } -> f' [] owner group others in
    let spec = Runtime'.Serialize.C.( basic_opt (1, (message (fun t -> Capability.to_proto t))) ^:: basic_opt (2, (message (fun t -> Capability.to_proto t))) ^:: basic_opt (3, (message (fun t -> Capability.to_proto t))) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions owner group others -> { owner; group; others } in
    let spec = Runtime'.Deserialize.C.( basic_opt (1, (message (fun t -> Capability.from_proto t))) ^:: basic_opt (2, (message (fun t -> Capability.from_proto t))) ^:: basic_opt (3, (message (fun t -> Capability.from_proto t))) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and FileStat : sig
  val name': unit -> string
  type t = { mode: Mode.t option; uid: int; gid: int; atime: string; ctime: string; mtime: string; size: string; link_path: string; is_directory: bool; is_file: bool; is_symlink: bool } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.FileStat"
  type t = { mode: Mode.t option; uid: int; gid: int; atime: string; ctime: string; mtime: string; size: string; link_path: string; is_directory: bool; is_file: bool; is_symlink: bool }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { mode; uid; gid; atime; ctime; mtime; size; link_path; is_directory; is_file; is_symlink } -> f' [] mode uid gid atime ctime mtime size link_path is_directory is_file is_symlink in
    let spec = Runtime'.Serialize.C.( basic_opt (1, (message (fun t -> Mode.to_proto t))) ^:: basic (2, int32_int, proto3) ^:: basic (3, int32_int, proto3) ^:: basic (4, string, proto3) ^:: basic (5, string, proto3) ^:: basic (6, string, proto3) ^:: basic (7, string, proto3) ^:: basic (11, string, proto3) ^:: basic (8, bool, proto3) ^:: basic (9, bool, proto3) ^:: basic (10, bool, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions mode uid gid atime ctime mtime size link_path is_directory is_file is_symlink -> { mode; uid; gid; atime; ctime; mtime; size; link_path; is_directory; is_file; is_symlink } in
    let spec = Runtime'.Deserialize.C.( basic_opt (1, (message (fun t -> Mode.from_proto t))) ^:: basic (2, int32_int, proto3) ^:: basic (3, int32_int, proto3) ^:: basic (4, string, proto3) ^:: basic (5, string, proto3) ^:: basic (6, string, proto3) ^:: basic (7, string, proto3) ^:: basic (11, string, proto3) ^:: basic (8, bool, proto3) ^:: basic (9, bool, proto3) ^:: basic (10, bool, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and FileItem : sig
  val name': unit -> string
  type t = { id: string; parent: string; name: string; full_path: string; stat: FileStat.t option; has_link_path: bool; link_path: string; marked: bool } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.FileItem"
  type t = { id: string; parent: string; name: string; full_path: string; stat: FileStat.t option; has_link_path: bool; link_path: string; marked: bool }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { id; parent; name; full_path; stat; has_link_path; link_path; marked } -> f' [] id parent name full_path stat has_link_path link_path marked in
    let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: basic (3, string, proto3) ^:: basic (4, string, proto3) ^:: basic_opt (5, (message (fun t -> FileStat.to_proto t))) ^:: basic (6, bool, proto3) ^:: basic (7, string, proto3) ^:: basic (8, bool, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions id parent name full_path stat has_link_path link_path marked -> { id; parent; name; full_path; stat; has_link_path; link_path; marked } in
    let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: basic (3, string, proto3) ^:: basic (4, string, proto3) ^:: basic_opt (5, (message (fun t -> FileStat.from_proto t))) ^:: basic (6, bool, proto3) ^:: basic (7, string, proto3) ^:: basic (8, bool, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and FileList : sig
  val name': unit -> string
  type t = { id: string; location: string; items: FileItem.t list; sort_order: Imported'modules.Types.SortType.t } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.FileList"
  type t = { id: string; location: string; items: FileItem.t list; sort_order: Imported'modules.Types.SortType.t }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { id; location; items; sort_order } -> f' [] id location items sort_order in
    let spec = Runtime'.Serialize.C.( basic (3, string, proto3) ^:: basic (1, string, proto3) ^:: repeated (2, (message (fun t -> FileItem.to_proto t)), not_packed) ^:: basic (4, (enum Imported'modules.Types.SortType.to_int), proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions id location items sort_order -> { id; location; items; sort_order } in
    let spec = Runtime'.Deserialize.C.( basic (3, string, proto3) ^:: basic (1, string, proto3) ^:: repeated (2, (message (fun t -> FileItem.from_proto t)), not_packed) ^:: basic (4, (enum Imported'modules.Types.SortType.from_int), proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and LocationHistory : sig
  val name': unit -> string
  type t = { records: LocationRecord.t list; max_record_number: int } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.LocationHistory"
  type t = { records: LocationRecord.t list; max_record_number: int }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { records; max_record_number } -> f' [] records max_record_number in
    let spec = Runtime'.Serialize.C.( repeated (1, (message (fun t -> LocationRecord.to_proto t)), not_packed) ^:: basic (2, int32_int, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions records max_record_number -> { records; max_record_number } in
    let spec = Runtime'.Deserialize.C.( repeated (1, (message (fun t -> LocationRecord.from_proto t)), not_packed) ^:: basic (2, int32_int, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and LocationRecord : sig
  val name': unit -> string
  type t = { location: string; timestamp: string } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.LocationRecord"
  type t = { location: string; timestamp: string }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { location; timestamp } -> f' [] location timestamp in
    let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions location timestamp -> { location; timestamp } in
    let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and FileWindow : sig
  val name': unit -> string
  type t = { file_list: FileList.t option; history: LocationHistory.t option } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.FileWindow"
  type t = { file_list: FileList.t option; history: LocationHistory.t option }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { file_list; history } -> f' [] file_list history in
    let spec = Runtime'.Serialize.C.( basic_opt (1, (message (fun t -> FileList.to_proto t))) ^:: basic_opt (2, (message (fun t -> LocationHistory.to_proto t))) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions file_list history -> { file_list; history } in
    let spec = Runtime'.Deserialize.C.( basic_opt (1, (message (fun t -> FileList.from_proto t))) ^:: basic_opt (2, (message (fun t -> LocationHistory.from_proto t))) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and Filer : sig
  val name': unit -> string
  type t = { left_file_window: FileWindow.t option; right_file_window: FileWindow.t option } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.Filer"
  type t = { left_file_window: FileWindow.t option; right_file_window: FileWindow.t option }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { left_file_window; right_file_window } -> f' [] left_file_window right_file_window in
    let spec = Runtime'.Serialize.C.( basic_opt (1, (message (fun t -> FileWindow.to_proto t))) ^:: basic_opt (2, (message (fun t -> FileWindow.to_proto t))) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions left_file_window right_file_window -> { left_file_window; right_file_window } in
    let spec = Runtime'.Deserialize.C.( basic_opt (1, (message (fun t -> FileWindow.from_proto t))) ^:: basic_opt (2, (message (fun t -> FileWindow.from_proto t))) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and InitializeRequest : sig
  val name': unit -> string
  type t = { left_location: string; right_location: string } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.InitializeRequest"
  type t = { left_location: string; right_location: string }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { left_location; right_location } -> f' [] left_location right_location in
    let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions left_location right_location -> { left_location; right_location } in
    let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and InitializeResponse : sig
  val name': unit -> string
  type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.InitializeResponse"
  type t = unit[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f () -> f [] in
    let spec = Runtime'.Serialize.C.( nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extension -> () in
    let spec = Runtime'.Deserialize.C.( nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and ReloadAllRequest : sig
  val name': unit -> string
  type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.ReloadAllRequest"
  type t = unit[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f () -> f [] in
    let spec = Runtime'.Serialize.C.( nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extension -> () in
    let spec = Runtime'.Deserialize.C.( nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and ReloadAllResponse : sig
  val name': unit -> string
  type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.ReloadAllResponse"
  type t = unit[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f () -> f [] in
    let spec = Runtime'.Serialize.C.( nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extension -> () in
    let spec = Runtime'.Deserialize.C.( nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and MoveLocationRequest : sig
  val name': unit -> string
  type t = { location: string; side: Side.t } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.MoveLocationRequest"
  type t = { location: string; side: Side.t }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { location; side } -> f' [] location side in
    let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: basic (2, (enum Side.to_int), proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions location side -> { location; side } in
    let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: basic (2, (enum Side.from_int), proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and MoveLocationResponse : sig
  val name': unit -> string
  type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.MoveLocationResponse"
  type t = unit[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f () -> f [] in
    let spec = Runtime'.Serialize.C.( nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extension -> () in
    let spec = Runtime'.Deserialize.C.( nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and UpDirectoryRequest : sig
  val name': unit -> string
  type t = { side: Side.t } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.UpDirectoryRequest"
  type t = { side: Side.t }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { side } -> f' [] side in
    let spec = Runtime'.Serialize.C.( basic (1, (enum Side.to_int), proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions side -> { side } in
    let spec = Runtime'.Deserialize.C.( basic (1, (enum Side.from_int), proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and UpDirectoryResponse : sig
  val name': unit -> string
  type t = { moved: bool } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.UpDirectoryResponse"
  type t = { moved: bool }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { moved } -> f' [] moved in
    let spec = Runtime'.Serialize.C.( basic (1, bool, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions moved -> { moved } in
    let spec = Runtime'.Deserialize.C.( basic (1, bool, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and OpenFileItemRequest : sig
  val name': unit -> string
  type t = { file_item_id: string; side: Side.t } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.OpenFileItemRequest"
  type t = { file_item_id: string; side: Side.t }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { file_item_id; side } -> f' [] file_item_id side in
    let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: basic (2, (enum Side.to_int), proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions file_item_id side -> { file_item_id; side } in
    let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: basic (2, (enum Side.from_int), proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and OpenFileItemResponse : sig
  module rec OpenResult : sig
    type t = NOT_IMPLEMENTED | DIRECTORY_OPENED [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_int: t -> int
    val from_int: int -> (t, [> Runtime'.Result.error]) result
  end
  val name': unit -> string
  type t = { result: OpenFileItemResponse.OpenResult.t } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  module rec OpenResult : sig
    type t = NOT_IMPLEMENTED | DIRECTORY_OPENED [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_int: t -> int
    val from_int: int -> (t, [> Runtime'.Result.error]) result
  end = struct 
    type t = NOT_IMPLEMENTED | DIRECTORY_OPENED [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    let to_int = function
      | NOT_IMPLEMENTED -> 0
      | DIRECTORY_OPENED -> 1
    
    let from_int = function
      | 0 -> Ok NOT_IMPLEMENTED
      | 1 -> Ok DIRECTORY_OPENED
      | n -> Error (`Unknown_enum_value n)
    
  end
  let name' () = "filer.OpenFileItemResponse"
  type t = { result: OpenFileItemResponse.OpenResult.t }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { result } -> f' [] result in
    let spec = Runtime'.Serialize.C.( basic (1, (enum OpenFileItemResponse.OpenResult.to_int), proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions result -> { result } in
    let spec = Runtime'.Deserialize.C.( basic (1, (enum OpenFileItemResponse.OpenResult.from_int), proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and ToggleMarkOfItemRequest : sig
  val name': unit -> string
  type t = { item_id: string; side: Side.t } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.ToggleMarkOfItemRequest"
  type t = { item_id: string; side: Side.t }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { item_id; side } -> f' [] item_id side in
    let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: basic (2, (enum Side.to_int), proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions item_id side -> { item_id; side } in
    let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: basic (2, (enum Side.from_int), proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and ToggleMarkOfItemResponse : sig
  val name': unit -> string
  type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.ToggleMarkOfItemResponse"
  type t = unit[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f () -> f [] in
    let spec = Runtime'.Serialize.C.( nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extension -> () in
    let spec = Runtime'.Deserialize.C.( nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and CopyUserDecisionRequest : sig
  val name': unit -> string
  type t = { item: FileItem.t option } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.CopyUserDecisionRequest"
  type t = { item: FileItem.t option }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { item } -> f' [] item in
    let spec = Runtime'.Serialize.C.( basic_opt (1, (message (fun t -> FileItem.to_proto t))) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions item -> { item } in
    let spec = Runtime'.Deserialize.C.( basic_opt (1, (message (fun t -> FileItem.from_proto t))) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and CopyUserDecisionResponse : sig
  val name': unit -> string
  type t = { action: Action.t; new_name: string } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.CopyUserDecisionResponse"
  type t = { action: Action.t; new_name: string }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { action; new_name } -> f' [] action new_name in
    let spec = Runtime'.Serialize.C.( basic (1, (enum Action.to_int), proto3) ^:: basic (2, string, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions action new_name -> { action; new_name } in
    let spec = Runtime'.Deserialize.C.( basic (1, (enum Action.from_int), proto3) ^:: basic (2, string, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and MoveUserDecisionRequest : sig
  val name': unit -> string
  type t = { item: FileItem.t option } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.MoveUserDecisionRequest"
  type t = { item: FileItem.t option }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { item } -> f' [] item in
    let spec = Runtime'.Serialize.C.( basic_opt (1, (message (fun t -> FileItem.to_proto t))) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions item -> { item } in
    let spec = Runtime'.Deserialize.C.( basic_opt (1, (message (fun t -> FileItem.from_proto t))) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and MoveUserDecisionResponse : sig
  val name': unit -> string
  type t = { action: Action.t; new_name: string } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.MoveUserDecisionResponse"
  type t = { action: Action.t; new_name: string }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { action; new_name } -> f' [] action new_name in
    let spec = Runtime'.Serialize.C.( basic (1, (enum Action.to_int), proto3) ^:: basic (2, string, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions action new_name -> { action; new_name } in
    let spec = Runtime'.Deserialize.C.( basic (1, (enum Action.from_int), proto3) ^:: basic (2, string, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and DeleteUserDecisionRequest : sig
  val name': unit -> string
  type t = { item: FileItem.t option } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.DeleteUserDecisionRequest"
  type t = { item: FileItem.t option }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { item } -> f' [] item in
    let spec = Runtime'.Serialize.C.( basic_opt (1, (message (fun t -> FileItem.to_proto t))) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions item -> { item } in
    let spec = Runtime'.Deserialize.C.( basic_opt (1, (message (fun t -> FileItem.from_proto t))) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and DeleteUserDecisionResponse : sig
  val name': unit -> string
  type t = { confirmed: bool } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.DeleteUserDecisionResponse"
  type t = { confirmed: bool }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { confirmed } -> f' [] confirmed in
    let spec = Runtime'.Serialize.C.( basic (1, bool, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions confirmed -> { confirmed } in
    let spec = Runtime'.Deserialize.C.( basic (1, bool, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and UpdatedNotificationRequest : sig
  val name': unit -> string
  type t = { filer: Filer.t option } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.UpdatedNotificationRequest"
  type t = { filer: Filer.t option }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { filer } -> f' [] filer in
    let spec = Runtime'.Serialize.C.( basic_opt (1, (message (fun t -> Filer.to_proto t))) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions filer -> { filer } in
    let spec = Runtime'.Deserialize.C.( basic_opt (1, (message (fun t -> Filer.from_proto t))) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and UpdatedNotificationResponse : sig
  val name': unit -> string
  type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.UpdatedNotificationResponse"
  type t = unit[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f () -> f [] in
    let spec = Runtime'.Serialize.C.( nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extension -> () in
    let spec = Runtime'.Deserialize.C.( nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and UpdatedFileWindowNotificationRequest : sig
  val name': unit -> string
  type t = { file_window: FileWindow.t option; side: Side.t } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.UpdatedFileWindowNotificationRequest"
  type t = { file_window: FileWindow.t option; side: Side.t }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { file_window; side } -> f' [] file_window side in
    let spec = Runtime'.Serialize.C.( basic_opt (1, (message (fun t -> FileWindow.to_proto t))) ^:: basic (2, (enum Side.to_int), proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions file_window side -> { file_window; side } in
    let spec = Runtime'.Deserialize.C.( basic_opt (1, (message (fun t -> FileWindow.from_proto t))) ^:: basic (2, (enum Side.from_int), proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and UpdatedFileWindowNotificationResponse : sig
  val name': unit -> string
  type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "filer.UpdatedFileWindowNotificationResponse"
  type t = unit[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f () -> f [] in
    let spec = Runtime'.Serialize.C.( nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extension -> () in
    let spec = Runtime'.Deserialize.C.( nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end