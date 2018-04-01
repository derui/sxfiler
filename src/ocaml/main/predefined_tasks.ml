module N = Jsoo_node
module C = Sxfiler_common
module S = C.State
module T = C.Types

(** Task to copy files to other pane *)
module Copy = struct
  type t = {
    fs: (module N.Fs_intf.Instance);
  }

  let copy_file (module Fs: N.Fs_intf.S) ~src ~dest =
    let open Lwt.Infix in
    Fs.copy_file ~src ~dest () >>= Lwt.wrap1 (fun ret ->
        match ret with
        | Ok () -> T.Task_result.(Ok Payload_copy)
        | Error `JsooSystemError e -> begin
            let error = Js.to_string e##.message in
            T.Task_result.(of_error error)
          end
      )

  let execute {fs} state =
    let active_pane = S.active_pane state
    and inactive_pane = S.inactive_pane state in
    let sources = S.Pane.selected_files active_pane in

    Lwt_list.fold_left_s (fun ret src ->
        let src = src.T.File_stat.filename in
        let dest = inactive_pane.T.Pane.directory in
        let filename = N.Path.basename src in
        let dest = N.Path.resolve [dest; filename] in

        let module Fs = N.Fs.Make(val fs) in
        copy_file (module Fs) ~src ~dest
      ) T.Task_result.(Ok Payload_copy) sources
end

(** Task to delete objects that are marked *)
module Delete = struct
  type t = {
    fs: (module N.Fs_intf.Instance);
  }

  let execute {fs} state =
    let active_pane = S.active_pane state in
    let files = S.Pane.selected_files active_pane in

    Lwt_list.fold_left_s (fun _ file ->
        let file = file.T.File_stat.filename in
        let module Fs = N.Fs.Make(val fs) in
        match Fs.remove_sync file with
        | Ok () -> Lwt.return @@ T.Task_result.(Ok Payload_delete)
        | Error `JsooSystemError e -> Lwt.return @@ T.Task_result.(of_error @@ Js.to_string e##.message)
      ) T.Task_result.(Ok Payload_delete) files
end

(** Task to move objects that are marked to other pane. *)
module Move = struct
  type t = {
    fs: (module N.Fs_intf.Instance);
  }

  let execute {fs} state =
    let active_pane = S.active_pane state
    and inactive_pane = S.inactive_pane state in
    let sources = S.Pane.selected_files active_pane in
    let dest = inactive_pane.T.Pane.directory in

    Lwt_list.fold_left_s (fun _ src ->
        let src = src.T.File_stat.filename in
        let filename = Filename.basename src in
        let dest = N.Path.resolve [dest; filename] in
        let module Fs = N.Fs.Make(val fs) in
        match Fs.renameSync src dest with
        | Ok () -> Lwt.return @@ T.Task_result.(Ok Payload_move)
        | Error `JsooSystemError e -> Lwt.return @@ T.Task_result.(of_error @@ Js.to_string e##.message)
      ) T.Task_result.(Ok Payload_move) sources

end

(** Task to rename object currently selected *)
module Rename = struct
  type t = {
    fs: (module N.Fs_intf.Instance);
    new_name: string;
  }

  let execute {fs;new_name} state =
    let module P = C.Types.Pane in
    let pane = S.active_pane state in

    let open Minimal_monadic_caml.Option.Infix in
    let item = P.find_item ~id:pane.P.focused_item pane in
    match item with
    | None -> Lwt.return @@ T.Task_result.(of_error "Not found current focused object")
    | Some src ->
      let module Fs = N.Fs.Make(val fs) in
      let src' = N.Path.join [src.T.File_stat.directory;src.T.File_stat.filename] in
      let new_name = N.Path.join [src.T.File_stat.directory; new_name] in
      if new_name = src' then
        Lwt.return @@ T.Task_result.(Ok Payload_rename)
      else match Fs.renameSync src' new_name with
        | Ok () -> Lwt.return @@ T.Task_result.(Ok Payload_rename)
        | Error `JsooSystemError e -> Lwt.return @@ T.Task_result.(of_error @@ Js.to_string e##.message)

end

(** Task to make directory that user specified *)
module Mkdir = struct
  type t = {
    fs: (module N.Fs_intf.Instance);
    dir_name: string;
  }

  let execute {fs;dir_name} state =
    let pane = S.active_pane state in
    let pane_dir = pane.T.Pane.directory in

    let module Fs = N.Fs.Make(val fs) in
    let dir_name = N.Path.join [pane_dir; dir_name] in

    let open Minimal_monadic_caml.Result.Infix in
    match Fs.mkdirSync dir_name with
    | Ok () -> Lwt.return @@ T.Task_result.(Ok Payload_mkdir)
    | Error `JsooSystemError e ->
      Lwt.return @@ T.Task_result.(of_error @@ Js.to_string e##.message)
end

(** Task to change permission of objects that are marked by user*)
module Change_permission = struct
  type t = {
    fs: (module N.Fs_intf.Instance);
    permission: int;
  }

  let execute {fs;permission} state =
    let sources = S.active_pane state |> S.Pane.selected_files in

    Lwt_list.fold_left_s (fun _ src ->
        let module Fs = N.Fs.Make(val fs) in
        let path = N.Path.join [src.T.File_stat.directory;src.T.File_stat.filename] in

        match Fs.chmodSync path permission with
        | Ok () -> Lwt.return @@ T.Task_result.(Ok Payload_change_permission)
        | Error `JsooSystemError e -> Lwt.return @@ T.Task_result.(of_error @@ Js.to_string e##.message)
      ) T.Task_result.(Ok Payload_rename) sources
end

type env = {
  fs: (module N.Fs_intf.Instance);
}

let of_request: env -> T.Task_request.t -> (module C.Task_intf.Task_instance) = fun env req ->
  let module I = C.Task_intf in
  match req with
  | T.Task_request.Copy -> (module struct
                             module Task = Copy
                             let instance = {Copy.fs = env.fs}
                           end : I.Task_instance)
  | Delete -> (module struct
                module Task = Delete
                let instance = {Delete.fs = env.fs}
              end: I.Task_instance)
  | Move -> (module struct
              module Task = Move
              let instance = {Move.fs = env.fs}
            end: I.Task_instance)
  | Rename s -> (module struct
                  module Task = Rename
                  let instance = {Rename.fs = env.fs; new_name = Js.to_string s}
                end: I.Task_instance)
  | Mkdir d -> (module struct
                 module Task = Mkdir
                 let instance = {Mkdir.fs = env.fs; dir_name = Js.to_string d}
               end: I.Task_instance)
  | Change_permission permission ->
    (module struct
      module Task = Change_permission
      let instance = {Change_permission.fs = env.fs; permission}
    end: I.Task_instance)
