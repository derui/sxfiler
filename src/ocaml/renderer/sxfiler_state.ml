module T = Sxfiler_common.Std.Types
module FFI = Sxfiler_common.Std.Ffi
module Thread = Lwt

(* All state of this application *)
type t = {
  ipc: FFI.ipc Js.t;
  file_list: T.File_stat.t list;
  waiting: bool;
}

type message = Sxfiler_message.t

type command = message Thread.t

let equal = ( = )
let update t = function
  | _ -> failwith "not implemented"
