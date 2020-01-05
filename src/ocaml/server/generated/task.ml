(************************************************)
(*       AUTOGENERATED FILE - DO NOT EDIT!      *)
(************************************************)
(* Generated by: ocaml-protoc-plugin            *)
(* https://github.com/issuu/ocaml-protoc-plugin *)
(************************************************)
(* Source: task.proto Syntax: proto3 Parameters: annot='[@@deriving eq, show, protocol
   ~driver:(module Protocol_conv_json.Json)]' debug=false opens=[] int64_as_int=true
   int32_as_int=true fixed_as_int=false singleton_record=true *)
module rec ReplyType : sig
  type t =
    | Overwrite
    | Rename
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_int : t -> int
  val from_int : int -> t Ocaml_protoc_plugin.Result.t
end = struct
  type t =
    | Overwrite
    | Rename
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_int = function Overwrite -> 0 | Rename -> 1
  let from_int = function 0 -> Ok Overwrite | 1 -> Ok Rename | n -> Error (`Unknown_enum_value n)
end

and TaskReply : sig
  module rec Rename : sig
    val name' : unit -> string

    type t = { newName : string }
    [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

    val to_proto : t -> Ocaml_protoc_plugin.Writer.t
    val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
  end

  val name' : unit -> string

  type t = {
    type' : ReplyType.t;
    reply : [ `Overwrite of bool | `Rename of Rename.t ];
    taskId : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  module rec Rename : sig
    val name' : unit -> string

    type t = { newName : string }
    [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

    val to_proto : t -> Ocaml_protoc_plugin.Writer.t
    val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
  end = struct
    let name' () = "Task.TaskReply.Rename"

    type t = { newName : string }
    [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

    let to_proto =
      let apply ~f:f' { newName } = f' newName in
      let spec = Ocaml_protoc_plugin.Serialize.C.(basic (1, string, proto3) ^:: nil) in
      let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
      fun t -> apply ~f:(serialize ()) t

    let from_proto =
      let constructor newName = { newName } in
      let spec = Ocaml_protoc_plugin.Deserialize.C.(basic (1, string, proto3) ^:: nil) in
      let deserialize = Ocaml_protoc_plugin.Deserialize.deserialize spec constructor in
      fun writer -> deserialize writer
  end

  let name' () = "Task.TaskReply"

  type t = {
    type' : ReplyType.t;
    reply : [ `Overwrite of bool | `Rename of Rename.t ];
    taskId : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { type'; reply; taskId } = f' type' reply taskId in
    let spec =
      Ocaml_protoc_plugin.Serialize.C.(
        basic (1, enum ReplyType.to_int, proto3)
        ^:: oneof (function
              | `Overwrite v -> oneof_elem (2, bool, v)
              | `Rename v -> oneof_elem (3, message Rename.to_proto, v))
        ^:: basic (4, string, proto3)
        ^:: nil)
    in
    let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
    fun t -> apply ~f:(serialize ()) t

  let from_proto =
    let constructor type' reply taskId = { type'; reply; taskId } in
    let spec =
      Ocaml_protoc_plugin.Deserialize.C.(
        basic (1, enum ReplyType.from_int, proto3)
        ^:: oneof
              [
                oneof_elem (2, bool, fun v -> `Overwrite v);
                oneof_elem (3, message Rename.from_proto, fun v -> `Rename v);
              ]
        ^:: basic (4, string, proto3)
        ^:: nil)
    in
    let deserialize = Ocaml_protoc_plugin.Deserialize.deserialize spec constructor in
    fun writer -> deserialize writer
end

and TaskSuggestion : sig
  val name' : unit -> string

  type t = {
    suggestions : ReplyType.t list;
    itemName : string;
    taskId : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Task.TaskSuggestion"

  type t = {
    suggestions : ReplyType.t list;
    itemName : string;
    taskId : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { suggestions; itemName; taskId } = f' suggestions itemName taskId in
    let spec =
      Ocaml_protoc_plugin.Serialize.C.(
        repeated (1, enum ReplyType.to_int, packed)
        ^:: basic (2, string, proto3)
        ^:: basic (3, string, proto3)
        ^:: nil)
    in
    let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
    fun t -> apply ~f:(serialize ()) t

  let from_proto =
    let constructor suggestions itemName taskId = { suggestions; itemName; taskId } in
    let spec =
      Ocaml_protoc_plugin.Deserialize.C.(
        repeated (1, enum ReplyType.from_int, packed)
        ^:: basic (2, string, proto3)
        ^:: basic (3, string, proto3)
        ^:: nil)
    in
    let deserialize = Ocaml_protoc_plugin.Deserialize.deserialize spec constructor in
    fun writer -> deserialize writer
end

and TaskSendReplyRequest : sig
  val name' : unit -> string

  type t = { reply : TaskReply.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Task.TaskSendReplyRequest"

  type t = { reply : TaskReply.t option }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { reply } = f' reply in
    let spec =
      Ocaml_protoc_plugin.Serialize.C.(basic_opt (1, message TaskReply.to_proto) ^:: nil)
    in
    let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
    fun t -> apply ~f:(serialize ()) t

  let from_proto =
    let constructor reply = { reply } in
    let spec =
      Ocaml_protoc_plugin.Deserialize.C.(basic_opt (1, message TaskReply.from_proto) ^:: nil)
    in
    let deserialize = Ocaml_protoc_plugin.Deserialize.deserialize spec constructor in
    fun writer -> deserialize writer
end

and TaskSendReplyResponse : sig
  val name' : unit -> string

  type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Task.TaskSendReplyResponse"

  type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f () = f in
    let spec = Ocaml_protoc_plugin.Serialize.C.(nil) in
    let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
    fun t -> apply ~f:(serialize ()) t

  let from_proto =
    let constructor = () in
    let spec = Ocaml_protoc_plugin.Deserialize.C.(nil) in
    let deserialize = Ocaml_protoc_plugin.Deserialize.deserialize spec constructor in
    fun writer -> deserialize writer
end

and TaskCancelRequest : sig
  val name' : unit -> string

  type t = { taskId : string }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Task.TaskCancelRequest"

  type t = { taskId : string }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { taskId } = f' taskId in
    let spec = Ocaml_protoc_plugin.Serialize.C.(basic (1, string, proto3) ^:: nil) in
    let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
    fun t -> apply ~f:(serialize ()) t

  let from_proto =
    let constructor taskId = { taskId } in
    let spec = Ocaml_protoc_plugin.Deserialize.C.(basic (1, string, proto3) ^:: nil) in
    let deserialize = Ocaml_protoc_plugin.Deserialize.deserialize spec constructor in
    fun writer -> deserialize writer
end

and TaskCancelResponse : sig
  val name' : unit -> string

  type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Task.TaskCancelResponse"

  type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f () = f in
    let spec = Ocaml_protoc_plugin.Serialize.C.(nil) in
    let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
    fun t -> apply ~f:(serialize ()) t

  let from_proto =
    let constructor = () in
    let spec = Ocaml_protoc_plugin.Deserialize.C.(nil) in
    let deserialize = Ocaml_protoc_plugin.Deserialize.deserialize spec constructor in
    fun writer -> deserialize writer
end
