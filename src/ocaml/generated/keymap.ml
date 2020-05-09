(************************************************)
(*       AUTOGENERATED FILE - DO NOT EDIT!      *)
(************************************************)
(* Generated by: ocaml-protoc-plugin            *)
(* https://github.com/issuu/ocaml-protoc-plugin *)
(************************************************)
(*
   Source: keymap.proto
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
module Keymap = struct
  module rec Keymap : sig
    val name': unit -> string
    type t = { bindings: Binding.t list } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "keymap.keymap.Keymap"
    type t = { bindings: Binding.t list }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    let to_proto =
      let apply = fun ~f:f' { bindings } -> f' [] bindings in
      let spec = Runtime'.Serialize.C.( repeated (1, (message (fun t -> Binding.to_proto t)), not_packed) ^:: nil ) in
      let serialize = Runtime'.Serialize.serialize [] (spec) in
      fun t -> apply ~f:serialize t
    
    let from_proto =
      let constructor = fun _extensions bindings -> { bindings } in
      let spec = Runtime'.Deserialize.C.( repeated (1, (message (fun t -> Binding.from_proto t)), not_packed) ^:: nil ) in
      let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
      fun writer -> deserialize writer |> Runtime'.Result.open_error
    
  end
  and Binding : sig
    val name': unit -> string
    type t = { key: string; action: string; contexts: string list } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "keymap.keymap.Binding"
    type t = { key: string; action: string; contexts: string list }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    let to_proto =
      let apply = fun ~f:f' { key; action; contexts } -> f' [] key action contexts in
      let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: repeated (3, string, packed) ^:: nil ) in
      let serialize = Runtime'.Serialize.serialize [] (spec) in
      fun t -> apply ~f:serialize t
    
    let from_proto =
      let constructor = fun _extensions key action contexts -> { key; action; contexts } in
      let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: repeated (3, string, packed) ^:: nil ) in
      let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
      fun writer -> deserialize writer |> Runtime'.Result.open_error
    
  end
  and GetRequest : sig
    val name': unit -> string
    type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "keymap.keymap.GetRequest"
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
    type t = { keymap: Keymap.t option } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "keymap.keymap.GetResponse"
    type t = { keymap: Keymap.t option }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    let to_proto =
      let apply = fun ~f:f' { keymap } -> f' [] keymap in
      let spec = Runtime'.Serialize.C.( basic_opt (1, (message (fun t -> Keymap.to_proto t))) ^:: nil ) in
      let serialize = Runtime'.Serialize.serialize [] (spec) in
      fun t -> apply ~f:serialize t
    
    let from_proto =
      let constructor = fun _extensions keymap -> { keymap } in
      let spec = Runtime'.Deserialize.C.( basic_opt (1, (message (fun t -> Keymap.from_proto t))) ^:: nil ) in
      let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
      fun writer -> deserialize writer |> Runtime'.Result.open_error
    
  end
  and ReloadRequest : sig
    val name': unit -> string
    type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "keymap.keymap.ReloadRequest"
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
  and ReloadResponse : sig
    val name': unit -> string
    type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "keymap.keymap.ReloadResponse"
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
  and AddKeyBindingRequest : sig
    val name': unit -> string
    type t = { key: string; action: string; contexts: string list } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "keymap.keymap.AddKeyBindingRequest"
    type t = { key: string; action: string; contexts: string list }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    let to_proto =
      let apply = fun ~f:f' { key; action; contexts } -> f' [] key action contexts in
      let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: repeated (3, string, packed) ^:: nil ) in
      let serialize = Runtime'.Serialize.serialize [] (spec) in
      fun t -> apply ~f:serialize t
    
    let from_proto =
      let constructor = fun _extensions key action contexts -> { key; action; contexts } in
      let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: repeated (3, string, packed) ^:: nil ) in
      let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
      fun writer -> deserialize writer |> Runtime'.Result.open_error
    
  end
  and AddKeyBindingResponse : sig
    val name': unit -> string
    type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "keymap.keymap.AddKeyBindingResponse"
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
  and RemoveKeyBindingRequest : sig
    val name': unit -> string
    type t = { key: string; contexts: string list } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "keymap.keymap.RemoveKeyBindingRequest"
    type t = { key: string; contexts: string list }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    let to_proto =
      let apply = fun ~f:f' { key; contexts } -> f' [] key contexts in
      let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: repeated (2, string, packed) ^:: nil ) in
      let serialize = Runtime'.Serialize.serialize [] (spec) in
      fun t -> apply ~f:serialize t
    
    let from_proto =
      let constructor = fun _extensions key contexts -> { key; contexts } in
      let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: repeated (2, string, packed) ^:: nil ) in
      let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
      fun writer -> deserialize writer |> Runtime'.Result.open_error
    
  end
  and RemoveKeyBindingResponse : sig
    val name': unit -> string
    type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
    val to_proto: t -> Runtime'.Writer.t
    val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
  end = struct 
    let name' () = "keymap.keymap.RemoveKeyBindingResponse"
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