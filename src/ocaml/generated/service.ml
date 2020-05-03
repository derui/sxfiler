(************************************************)
(*       AUTOGENERATED FILE - DO NOT EDIT!      *)
(************************************************)
(* Generated by: ocaml-protoc-plugin            *)
(* https://github.com/issuu/ocaml-protoc-plugin *)
(************************************************)
(*
   Source: service.proto
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
module rec Command : sig
  type t = UNKNOWN_COMMAND | FILER_INITIALIZE | FILER_RELOAD_ALL | FILER_MOVE_LOCATION | FILER_UPDATED | FILER_COPY_INTERACTION | FILER_MOVE_INTERACTION | FILER_DELETE_INTERACTION | KEYMAP_ADD_KEY_BINDING | KEYMAP_REMOVE_KEY_BINDING | KEYMAP_GET | KEYMAP_RELOAD | KEYMAP_UPDATED | FILER_OPEN_FILE_ITEM | CONFIGURATION_GET | FILER_UP_DIRECTORY | FILER_TOGGLE_MARK_OF_ITEM | FILER_UPDATED_FILE_WINDOW | COMPLETER_INITIALIZE | COMPLETER_COMPLETE | COMPLETER_NOTIFY_COMPLETED | FILER_MOVE [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_int: t -> int
  val from_int: int -> (t, [> Runtime'.Result.error]) result
end = struct 
  type t = UNKNOWN_COMMAND | FILER_INITIALIZE | FILER_RELOAD_ALL | FILER_MOVE_LOCATION | FILER_UPDATED | FILER_COPY_INTERACTION | FILER_MOVE_INTERACTION | FILER_DELETE_INTERACTION | KEYMAP_ADD_KEY_BINDING | KEYMAP_REMOVE_KEY_BINDING | KEYMAP_GET | KEYMAP_RELOAD | KEYMAP_UPDATED | FILER_OPEN_FILE_ITEM | CONFIGURATION_GET | FILER_UP_DIRECTORY | FILER_TOGGLE_MARK_OF_ITEM | FILER_UPDATED_FILE_WINDOW | COMPLETER_INITIALIZE | COMPLETER_COMPLETE | COMPLETER_NOTIFY_COMPLETED | FILER_MOVE [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_int = function
    | UNKNOWN_COMMAND -> 0
    | FILER_INITIALIZE -> 1
    | FILER_RELOAD_ALL -> 2
    | FILER_MOVE_LOCATION -> 3
    | FILER_UPDATED -> 4
    | FILER_COPY_INTERACTION -> 5
    | FILER_MOVE_INTERACTION -> 6
    | FILER_DELETE_INTERACTION -> 7
    | KEYMAP_ADD_KEY_BINDING -> 8
    | KEYMAP_REMOVE_KEY_BINDING -> 9
    | KEYMAP_GET -> 10
    | KEYMAP_RELOAD -> 11
    | KEYMAP_UPDATED -> 12
    | FILER_OPEN_FILE_ITEM -> 13
    | CONFIGURATION_GET -> 14
    | FILER_UP_DIRECTORY -> 15
    | FILER_TOGGLE_MARK_OF_ITEM -> 16
    | FILER_UPDATED_FILE_WINDOW -> 17
    | COMPLETER_INITIALIZE -> 18
    | COMPLETER_COMPLETE -> 19
    | COMPLETER_NOTIFY_COMPLETED -> 20
    | FILER_MOVE -> 21
  
  let from_int = function
    | 0 -> Ok UNKNOWN_COMMAND
    | 1 -> Ok FILER_INITIALIZE
    | 2 -> Ok FILER_RELOAD_ALL
    | 3 -> Ok FILER_MOVE_LOCATION
    | 4 -> Ok FILER_UPDATED
    | 5 -> Ok FILER_COPY_INTERACTION
    | 6 -> Ok FILER_MOVE_INTERACTION
    | 7 -> Ok FILER_DELETE_INTERACTION
    | 8 -> Ok KEYMAP_ADD_KEY_BINDING
    | 9 -> Ok KEYMAP_REMOVE_KEY_BINDING
    | 10 -> Ok KEYMAP_GET
    | 11 -> Ok KEYMAP_RELOAD
    | 12 -> Ok KEYMAP_UPDATED
    | 13 -> Ok FILER_OPEN_FILE_ITEM
    | 14 -> Ok CONFIGURATION_GET
    | 15 -> Ok FILER_UP_DIRECTORY
    | 16 -> Ok FILER_TOGGLE_MARK_OF_ITEM
    | 17 -> Ok FILER_UPDATED_FILE_WINDOW
    | 18 -> Ok COMPLETER_INITIALIZE
    | 19 -> Ok COMPLETER_COMPLETE
    | 20 -> Ok COMPLETER_NOTIFY_COMPLETED
    | 21 -> Ok FILER_MOVE
    | n -> Error (`Unknown_enum_value n)
  
end
and Status : sig
  type t = UNKNOWN_STATUS | SUCCESS | INVALID_REQUEST_PAYLOAD | COMMAND_FAILED [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_int: t -> int
  val from_int: int -> (t, [> Runtime'.Result.error]) result
end = struct 
  type t = UNKNOWN_STATUS | SUCCESS | INVALID_REQUEST_PAYLOAD | COMMAND_FAILED [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_int = function
    | UNKNOWN_STATUS -> 0
    | SUCCESS -> 1
    | INVALID_REQUEST_PAYLOAD -> 2
    | COMMAND_FAILED -> 3
  
  let from_int = function
    | 0 -> Ok UNKNOWN_STATUS
    | 1 -> Ok SUCCESS
    | 2 -> Ok INVALID_REQUEST_PAYLOAD
    | 3 -> Ok COMMAND_FAILED
    | n -> Error (`Unknown_enum_value n)
  
end
and Request : sig
  val name': unit -> string
  type t = { id: string; command: Command.t; payload: bytes } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "service.Request"
  type t = { id: string; command: Command.t; payload: bytes }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { id; command; payload } -> f' [] id command payload in
    let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: basic (2, (enum Command.to_int), proto3) ^:: basic (3, bytes, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions id command payload -> { id; command; payload } in
    let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: basic (2, (enum Command.from_int), proto3) ^:: basic (3, bytes, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and ErrorDetail : sig
  val name': unit -> string
  type t = { field: string; error_message: string } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "service.ErrorDetail"
  type t = { field: string; error_message: string }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { field; error_message } -> f' [] field error_message in
    let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions field error_message -> { field; error_message } in
    let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: basic (2, string, proto3) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and Error : sig
  val name': unit -> string
  type t = { status: int; error_message: string; details: ErrorDetail.t list } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "service.Error"
  type t = { status: int; error_message: string; details: ErrorDetail.t list }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { status; error_message; details } -> f' [] status error_message details in
    let spec = Runtime'.Serialize.C.( basic (1, int32_int, proto3) ^:: basic (2, string, proto3) ^:: repeated (3, (message (fun t -> ErrorDetail.to_proto t)), not_packed) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions status error_message details -> { status; error_message; details } in
    let spec = Runtime'.Deserialize.C.( basic (1, int32_int, proto3) ^:: basic (2, string, proto3) ^:: repeated (3, (message (fun t -> ErrorDetail.from_proto t)), not_packed) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
and Response : sig
  val name': unit -> string
  type t = { id: string; status: Status.t; payload: bytes; error: Error.t option } [@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  val to_proto: t -> Runtime'.Writer.t
  val from_proto: Runtime'.Reader.t -> (t, [> Runtime'.Result.error]) result
end = struct 
  let name' () = "service.Response"
  type t = { id: string; status: Status.t; payload: bytes; error: Error.t option }[@@deriving eq, show, protocol ~driver:(module Protocol_conv_json.Json)]
  let to_proto =
    let apply = fun ~f:f' { id; status; payload; error } -> f' [] id status payload error in
    let spec = Runtime'.Serialize.C.( basic (1, string, proto3) ^:: basic (5, (enum Status.to_int), proto3) ^:: basic (2, bytes, proto3) ^:: basic_opt (4, (message (fun t -> Error.to_proto t))) ^:: nil ) in
    let serialize = Runtime'.Serialize.serialize [] (spec) in
    fun t -> apply ~f:serialize t
  
  let from_proto =
    let constructor = fun _extensions id status payload error -> { id; status; payload; error } in
    let spec = Runtime'.Deserialize.C.( basic (1, string, proto3) ^:: basic (5, (enum Status.from_int), proto3) ^:: basic (2, bytes, proto3) ^:: basic_opt (4, (message (fun t -> Error.from_proto t))) ^:: nil ) in
    let deserialize = Runtime'.Deserialize.deserialize [] spec constructor in
    fun writer -> deserialize writer |> Runtime'.Result.open_error
  
end
module FilerService = struct
  let initialize = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let reloadAll = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let moveLocation = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let openFileItem = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let upDirectory = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let toggleMarkOfItem = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let move = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let requireUserDecisionForCopy = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let requireUserDecisionForMove = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let requireUserDecisionForDelete = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let notifyUpdated = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let notifyUpdatedFileWindow = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
end
module KeymapService = struct
  let get = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let reload = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let addKeyBinding = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let removeKeyBinding = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let notifyUpdated = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
end
module ConfigurationService = struct
  let get = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
end
module CompleterService = struct
  let initialize = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let complete = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
  let notifyCompleted = 
    ( (module Request : Runtime'.Service.Message with type t = Request.t ), 
    (module Response : Runtime'.Service.Message with type t = Response.t ) ) 
end