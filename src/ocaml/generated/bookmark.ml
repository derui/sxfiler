(************************************************)
(*       AUTOGENERATED FILE - DO NOT EDIT!      *)
(************************************************)
(* Generated by: ocaml-protoc-plugin            *)
(* https://github.com/issuu/ocaml-protoc-plugin *)
(************************************************)
(*
   Source: bookmark.proto
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
module rec Bookmark : sig
  val name': unit -> string
  type t = { name: string; path: string } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "bookmark.Bookmark"
  type t = { name: string; path: string }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { name; path } -> f' [] name path in
    let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions name path -> { name; path } in
    let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and BookmarkList : sig
  val name': unit -> string
  type t = { items: Bookmark.t list } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "bookmark.BookmarkList"
  type t = { items: Bookmark.t list }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { items } -> f' [] items in
    let spec = Runtime'.Serialize.C.( repeated (1, (message (fun t -> Bookmark.to_proto t)), not_packed) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions items -> { items } in
    let spec = Runtime'.Deserialize.C.( repeated (1, (message (fun t -> Bookmark.from_proto t)), not_packed) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and ListAllRequest : sig
  val name': unit -> string
  type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "bookmark.ListAllRequest"
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
and ListAllResponse : sig
  val name': unit -> string
  type t = { bookmarks: BookmarkList.t option } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "bookmark.ListAllResponse"
  type t = { bookmarks: BookmarkList.t option }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { bookmarks } -> f' [] bookmarks in
    let spec = Runtime'.Serialize.C.( basic_opt (1, (message (fun t -> BookmarkList.to_proto t))) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions bookmarks -> { bookmarks } in
    let spec = Runtime'.Deserialize.C.( basic_opt (1, (message (fun t -> BookmarkList.from_proto t))) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and RegisterRequest : sig
  val name': unit -> string
  type t = { path: string } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "bookmark.RegisterRequest"
  type t = { path: string }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { path } -> f' [] path in
    let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions path -> { path } in
    let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and RegisterResponse : sig
  val name': unit -> string
  type t = { bookmark: Bookmark.t option } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "bookmark.RegisterResponse"
  type t = { bookmark: Bookmark.t option }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { bookmark } -> f' [] bookmark in
    let spec = Runtime'.Serialize.C.( basic_opt (1, (message (fun t -> Bookmark.to_proto t))) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions bookmark -> { bookmark } in
    let spec = Runtime'.Deserialize.C.( basic_opt (1, (message (fun t -> Bookmark.from_proto t))) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and DeleteRequest : sig
  val name': unit -> string
  type t = { id: string } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "bookmark.DeleteRequest"
  type t = { id: string }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { id } -> f' [] id in
    let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions id -> { id } in
    let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and DeleteResponse : sig
  val name': unit -> string
  type t = { deleted_bookmarks: Bookmark.t option } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "bookmark.DeleteResponse"
  type t = { deleted_bookmarks: Bookmark.t option }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { deleted_bookmarks } -> f' [] deleted_bookmarks in
    let spec = Runtime'.Serialize.C.( basic_opt (1, (message (fun t -> Bookmark.to_proto t))) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions deleted_bookmarks -> { deleted_bookmarks } in
    let spec = Runtime'.Deserialize.C.( basic_opt (1, (message (fun t -> Bookmark.from_proto t))) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end