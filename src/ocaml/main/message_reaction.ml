
module E = Sxfiler_common.Event
module FFI = Sxfiler_common.Ffi
module M = Sxfiler_common.Message

exception Unhandled_promise

module type Fs = sig
  val resolve: unit -> FFI.Fs.t Js.t
end

module type S = sig
  module Fs : Fs

  val request_files_in_directory: string -> M.t Lwt.t
end

module Make(Fs:Fs) : S with module Fs = Fs = struct
  module Fs = Fs

  (** Handle request_files_in_directory message *)
  let request_files_in_directory path =
    let fs = Fs.resolve () in
    let path_ = Modules.path in
    let path = Js.string path in
    let absolute = path_##resolve Js.(array [|path|]) |> Js.to_string in

    let open Lwt.Infix in
    let lwt = File_list.get_file_stats ~fs absolute
      >>= (fun files ->
          let module M = Sxfiler_common.Message in
          Lwt.return @@ M.finish_files_in_directory (None, absolute, files)
        )
    in

    let lwt = Lwt.catch (fun () -> lwt) (fun err ->
        Firebug.console##log err;
        match err with
        | File_list.Not_directory f ->
          let module M = Sxfiler_common.Message in
          Lwt.return @@ M.finish_files_in_directory (Some err, absolute, [||])
        | _ -> raise Unhandled_promise
      )
    in
    lwt
end
