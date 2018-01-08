
module C = Sxfiler_common.Std.Const
module FFI = Sxfiler_common.Std.Ffi
module M = Sxfiler_renderer_modules

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

  val make: ipc:FFI.ipc Js.t -> runner:Sxfiler_flux_runner.t -> t
  (* Make ipc *)

  val send_to_main: channel:C.IPC_events.t -> t -> unit
  (* Send message to main ipc *)

end = struct
  type t = {
    ipc: FFI.ipc Js.t;
    runner: Sxfiler_flux_runner.t;
  }

  let on_finish_files_in_directory t ev (exn, path, list) =
    ()

  let make ~ipc ~runner =
    let t = {ipc;runner} in
    let listener ev v = on_finish_files_in_directory t ev v in
    C.IPC_events.(on ~channel:(`FINISH_FILES_IN_DIRECTORY (None, "", [||]) ) ~listener ipc);
    t

  let send_to_main ~channel t =
    C.IPC_events.send ~channel ~ipc:t.ipc
end
