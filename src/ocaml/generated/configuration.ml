(************************************************)
(*       AUTOGENERATED FILE - DO NOT EDIT!      *)
(************************************************)
(* Generated by: ocaml-protoc-plugin            *)
(* https://github.com/issuu/ocaml-protoc-plugin *)
(************************************************)
(*
   Source: configuration.proto
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
module Configuration = struct
  module rec Configuration : sig
    val name': unit -> string
    type t = { key: string list; json_value: string } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "configuration.configuration.Configuration"
    type t = { key: string list; json_value: string }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    let to_proto =
      let apply = fun ~f:f' { key; json_value } -> f' [] key json_value in
      let spec = Runtime'.Serialize.C.( repeated (1, string, packed) ^:: basic (2, string, proto3) ^:: nil ) in
      let serialize = Runtime'.Serialize.serialize [] (spec) in
      fun t -> apply ~f:serialize t
    
    let from_proto =
      let constructor = fun _extensions key json_value -> { key; json_value } in
      let spec = Runtime'.Deserialize.C.( repeated (1, string, packed) ^:: basic (2, string, proto3) ^:: nil ) in
      let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
      fun writer -> deserialize writer |> Runtime'.Result.open_error
    
  end
  and GetRequest : sig
    val name': unit -> string
    type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "configuration.configuration.GetRequest"
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
  and GetResponse : sig
    val name': unit -> string
    type t = { configurations: Configuration.t list } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "configuration.configuration.GetResponse"
    type t = { configurations: Configuration.t list }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    let to_proto =
      let apply = fun ~f:f' { configurations } -> f' [] configurations in
      let spec = Runtime'.Serialize.C.( repeated (1, (message (fun t -> Configuration.to_proto t)), not_packed) ^:: nil ) in
      let serialize = Runtime'.Serialize.serialize [] (spec) in
      fun t -> apply ~f:serialize t
    
    let from_proto =
      let constructor = fun _extensions configurations -> { configurations } in
      let spec = Runtime'.Deserialize.C.( repeated (1, (message (fun t -> Configuration.from_proto t)), not_packed) ^:: nil ) in
      let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
      fun writer -> deserialize writer |> Runtime'.Result.open_error
    
  end
  and UpdateRequest : sig
    val name': unit -> string
    type t = { key: string list; json_value: string } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "configuration.configuration.UpdateRequest"
    type t = { key: string list; json_value: string }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    let to_proto =
      let apply = fun ~f:f' { key; json_value } -> f' [] key json_value in
      let spec = Runtime'.Serialize.C.( repeated (1, string, packed) ^:: basic (2, string, proto3) ^:: nil ) in
      let serialize = Runtime'.Serialize.serialize [] (spec) in
      fun t -> apply ~f:serialize t
    
    let from_proto =
      let constructor = fun _extensions key json_value -> { key; json_value } in
      let spec = Runtime'.Deserialize.C.( repeated (1, string, packed) ^:: basic (2, string, proto3) ^:: nil ) in
      let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
      fun writer -> deserialize writer |> Runtime'.Result.open_error
    
  end
  and UpdateResponse : sig
    val name': unit -> string
    type t = { key: string list } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "configuration.configuration.UpdateResponse"
    type t = { key: string list }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    let to_proto =
      let apply = fun ~f:f' { key } -> f' [] key in
      let spec = Runtime'.Serialize.C.( repeated (1, string, packed) ^:: nil ) in
      let serialize = Runtime'.Serialize.serialize [] (spec) in
      fun t -> apply ~f:serialize t
    
    let from_proto =
      let constructor = fun _extensions key -> { key } in
      let spec = Runtime'.Deserialize.C.( repeated (1, string, packed) ^:: nil ) in
      let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
      fun writer -> deserialize writer |> Runtime'.Result.open_error
    
  end
  and UpdatedNotificationRequest : sig
    val name': unit -> string
    type t = { configurations: Configuration.t list } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "configuration.configuration.UpdatedNotificationRequest"
    type t = { configurations: Configuration.t list }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    let to_proto =
      let apply = fun ~f:f' { configurations } -> f' [] configurations in
      let spec = Runtime'.Serialize.C.( repeated (1, (message (fun t -> Configuration.to_proto t)), not_packed) ^:: nil ) in
      let serialize = Runtime'.Serialize.serialize [] (spec) in
      fun t -> apply ~f:serialize t
    
    let from_proto =
      let constructor = fun _extensions configurations -> { configurations } in
      let spec = Runtime'.Deserialize.C.( repeated (1, (message (fun t -> Configuration.from_proto t)), not_packed) ^:: nil ) in
      let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
      fun writer -> deserialize writer |> Runtime'.Result.open_error
    
  end
  and UpdatedNotificationResponse : sig
    val name': unit -> string
    type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "configuration.configuration.UpdatedNotificationResponse"
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
end