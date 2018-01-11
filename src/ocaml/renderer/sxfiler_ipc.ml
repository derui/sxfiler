
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
module Core : sig
  type t

  val make: ipc:FFI.ipc Js.t -> adapter:Sxfiler_action.t -> t
  (* Make ipc *)

  val send_to_main: channel:E.IPC.t -> t -> unit
  (* Send message to main ipc *)

end = struct
  type t = {
    ipc: FFI.ipc Js.t;
    adapter: Sxfiler_action.t;
  }

  let on_finish_files_in_directory t ev (exn, path, list) =
    Sxfiler_action.
    ()

  let make ~ipc ~adapter =
    let t = {ipc;adapter} in
    let listener ev v = on_finish_files_in_directory t ev v in
    E.IPC.(on ~target:Listener.finish_files_in_directory ~f:listener ipc);
    t

  let send_to_main ~channel t =
    E.IPC.send ~channel ~ipc:t.ipc
end
