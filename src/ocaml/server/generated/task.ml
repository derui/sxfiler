(************************************************)
(*       AUTOGENERATED FILE - DO NOT EDIT!      *)
(************************************************)
(* Generated by: ocaml-protoc-plugin            *)
(* https://github.com/issuu/ocaml-protoc-plugin *)
(************************************************)
(* Source: task.proto Syntax: proto3 Parameters: annot='[@@deriving eq, show, protocol
   ~driver:(module Protocol_conv_json.Json)]' debug=false opens=[] int64_as_int=true
   int32_as_int=true fixed_as_int=false singleton_record=false *)
module rec TaskReply : sig
  module rec Rename : sig
    val name' : unit -> string

    type t = string [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

    val to_proto : t -> Ocaml_protoc_plugin.Writer.t
    val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
  end

  val name' : unit -> string

  type t = {
    reply : [ `Overwrite of bool | `Rename of Rename.t ];
    taskId : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  module rec Rename : sig
    val name' : unit -> string

    type t = string [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

    val to_proto : t -> Ocaml_protoc_plugin.Writer.t
    val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
  end = struct
    let name' () = "Task.TaskReply.Rename"

    type t = string [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

    let to_proto =
      let apply ~f a = f a in
      let spec = Ocaml_protoc_plugin.Serialize.C.(basic (1, string, proto3) ^:: nil) in
      let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
      fun t -> apply ~f:(serialize ()) t

    let from_proto =
      let constructor a = a in
      let spec = Ocaml_protoc_plugin.Deserialize.C.(basic (1, string, proto3) ^:: nil) in
      let deserialize = Ocaml_protoc_plugin.Deserialize.deserialize spec constructor in
      fun writer -> deserialize writer
  end

  let name' () = "Task.TaskReply"

  type t = {
    reply : [ `Overwrite of bool | `Rename of Rename.t ];
    taskId : string;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { reply; taskId } = f' reply taskId in
    let spec =
      Ocaml_protoc_plugin.Serialize.C.(
        oneof (function
          | `Overwrite v -> oneof_elem (1, bool, v)
          | `Rename v -> oneof_elem (2, message Rename.to_proto, v))
        ^:: basic (3, string, proto3)
        ^:: nil)
    in
    let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
    fun t -> apply ~f:(serialize ()) t

  let from_proto =
    let constructor reply taskId = { reply; taskId } in
    let spec =
      Ocaml_protoc_plugin.Deserialize.C.(
        oneof
          [
            oneof_elem (1, bool, fun v -> `Overwrite v);
            oneof_elem (2, message Rename.from_proto, fun v -> `Rename v);
          ]
        ^:: basic (3, string, proto3)
        ^:: nil)
    in
    let deserialize = Ocaml_protoc_plugin.Deserialize.deserialize spec constructor in
    fun writer -> deserialize writer
end

and TaskSendReplyRequest : sig
  val name' : unit -> string

  type t = TaskReply.t option
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Task.TaskSendReplyRequest"

  type t = TaskReply.t option
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f a = f a in
    let spec =
      Ocaml_protoc_plugin.Serialize.C.(basic_opt (1, message TaskReply.to_proto) ^:: nil)
    in
    let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
    fun t -> apply ~f:(serialize ()) t

  let from_proto =
    let constructor a = a in
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

  type t = string [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Task.TaskCancelRequest"

  type t = string [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f a = f a in
    let spec = Ocaml_protoc_plugin.Serialize.C.(basic (1, string, proto3) ^:: nil) in
    let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
    fun t -> apply ~f:(serialize ()) t

  let from_proto =
    let constructor a = a in
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
