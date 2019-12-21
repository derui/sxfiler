(************************************************)
(*       AUTOGENERATED FILE - DO NOT EDIT!      *)
(************************************************)
(* Generated by: ocaml-protoc-plugin            *)
(* https://github.com/issuu/ocaml-protoc-plugin *)
(************************************************)
(* Source: bookmark.proto Syntax: proto3 Parameters: annot='[@@deriving eq, show, protocol
   ~driver:(module Protocol_conv_json.Json)]' debug=false opens=[] int64_as_int=true
   int32_as_int=true fixed_as_int=false singleton_record=false *)
module rec Bookmark : sig
  val name' : unit -> string

  type t = {
    id : string;
    path : string;
    order : int;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Bookmark.Bookmark"

  type t = {
    id : string;
    path : string;
    order : int;
  }
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f:f' { id; path; order } = f' id path order in
    let spec =
      Ocaml_protoc_plugin.Serialize.C.(
        basic (1, string, proto3)
        ^:: basic (2, string, proto3)
        ^:: basic (3, int32_int, proto3)
        ^:: nil)
    in
    let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
    fun t -> apply ~f:(serialize ()) t

  let from_proto =
    let constructor id path order = { id; path; order } in
    let spec =
      Ocaml_protoc_plugin.Deserialize.C.(
        basic (1, string, proto3)
        ^:: basic (2, string, proto3)
        ^:: basic (3, int32_int, proto3)
        ^:: nil)
    in
    let deserialize = Ocaml_protoc_plugin.Deserialize.deserialize spec constructor in
    fun writer -> deserialize writer
end

and ListAllRequest : sig
  val name' : unit -> string

  type t = unit [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Bookmark.ListAllRequest"

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

and ListAllResponse : sig
  val name' : unit -> string

  type t = Bookmark.t list [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Bookmark.ListAllResponse"

  type t = Bookmark.t list [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f a = f a in
    let spec =
      Ocaml_protoc_plugin.Serialize.C.(repeated (1, message Bookmark.to_proto, not_packed) ^:: nil)
    in
    let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
    fun t -> apply ~f:(serialize ()) t

  let from_proto =
    let constructor a = a in
    let spec =
      Ocaml_protoc_plugin.Deserialize.C.(
        repeated (1, message Bookmark.from_proto, not_packed) ^:: nil)
    in
    let deserialize = Ocaml_protoc_plugin.Deserialize.deserialize spec constructor in
    fun writer -> deserialize writer
end

and RegisterRequest : sig
  val name' : unit -> string

  type t = string [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Bookmark.RegisterRequest"

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

and RegisterResponse : sig
  val name' : unit -> string

  type t = Bookmark.t option
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Bookmark.RegisterResponse"

  type t = Bookmark.t option
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f a = f a in
    let spec = Ocaml_protoc_plugin.Serialize.C.(basic_opt (1, message Bookmark.to_proto) ^:: nil) in
    let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
    fun t -> apply ~f:(serialize ()) t

  let from_proto =
    let constructor a = a in
    let spec =
      Ocaml_protoc_plugin.Deserialize.C.(basic_opt (1, message Bookmark.from_proto) ^:: nil)
    in
    let deserialize = Ocaml_protoc_plugin.Deserialize.deserialize spec constructor in
    fun writer -> deserialize writer
end

and DeleteRequest : sig
  val name' : unit -> string

  type t = string [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Bookmark.DeleteRequest"

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

and DeleteResponse : sig
  val name' : unit -> string

  type t = Bookmark.t option
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  val to_proto : t -> Ocaml_protoc_plugin.Writer.t
  val from_proto : Ocaml_protoc_plugin.Reader.t -> t Ocaml_protoc_plugin.Result.t
end = struct
  let name' () = "Bookmark.DeleteResponse"

  type t = Bookmark.t option
  [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]

  let to_proto =
    let apply ~f a = f a in
    let spec = Ocaml_protoc_plugin.Serialize.C.(basic_opt (1, message Bookmark.to_proto) ^:: nil) in
    let serialize = Ocaml_protoc_plugin.Serialize.serialize spec in
    fun t -> apply ~f:(serialize ()) t

  let from_proto =
    let constructor a = a in
    let spec =
      Ocaml_protoc_plugin.Deserialize.C.(basic_opt (1, message Bookmark.from_proto) ^:: nil)
    in
    let deserialize = Ocaml_protoc_plugin.Deserialize.deserialize spec constructor in
    fun writer -> deserialize writer
end
