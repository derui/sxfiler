
module E = Sxfiler_common.Std.Event
module FFI = Sxfiler_common.Std.Ffi
module M = Sxfiler_modules

exception Unhandled_promise

(**
 * IPC event management for Main process.
 *
 * This module provide only event handlers for request event in {IPC_KEYS}.
 * User must not instance greater than two times, so user only initialize in Main class
 * for Main process.
*)
type t = {
  ipc: FFI.ipc Js.t;
  connector: Sxfiler_connector.t;
}

let on_finish_files_in_directory t ev (exn, path, list) =
  ()

let make ~ipc ~connector =
  let t = {ipc;connector} in
  let listener ev v = on_finish_files_in_directory t ev v in
  E.IPC.(on ~target:Listener.finish_files_in_directory ~f:listener ipc);
