module N = Jsoo_node
module C = Sxfiler_common
module S = C.State
module T = C.Types

module Copy = struct
  type t = {
    fs: (module N.Fs_intf.Instance);
  }

  let execute {fs} state =
    let active_pane = S.active_pane state
    and inactive_pane = S.inactive_pane state in
    let sources = S.Pane.selected_files active_pane in

    Lwt_list.fold_left_s (fun ret src ->
        let src = active_pane.T.Pane.selected_item in
        let dest = inactive_pane.T.Pane.directory in

        match src with
        | None -> Lwt.return @@ T.Task_result.of_error "Do not select any item"
        | Some src -> begin
            let src = src.T.File_stat.filename in
            let filename = N.Path.basename src in
            let dest = N.Path.resolve [dest; filename] in

            let module Fs = N.Fs.Make(val fs) in
            let open Lwt.Infix in
            Fs.copy_file ~src ~dest () >>= (fun ret ->
                match ret with
                | Ok _ -> Lwt.return @@ T.Task_result.(Ok Payload_copy)
                | Error err -> begin
                    match err with
                    | `FsCopyError err ->
                      let error = Js.to_string err##toString in
                      Lwt.return @@ T.Task_result.(of_error error)
                  end
              )
          end
      ) T.Task_result.(Ok Payload_copy) sources
end

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
        match Fs.unlinkSync file with
        | Ok _ -> Lwt.return @@ T.Task_result.(Ok Payload_delete)
        | Error _ -> Lwt.return @@ T.Task_result.(of_error "")
      ) T.Task_result.(Ok Payload_delete) files
end

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
        | Ok _ -> Lwt.return @@ T.Task_result.(Ok Payload_move)
        | Error _ -> Lwt.return @@ T.Task_result.(of_error "")
      ) T.Task_result.(Ok Payload_move) sources

end

module Rename = struct
  type t = {
    fs: (module N.Fs_intf.Instance);
    new_name: string;
  }

  let execute {fs;new_name} state =
    let sources = S.active_pane state |> S.Pane.selected_files in

    Lwt_list.fold_left_s (fun _ src ->
        let module Fs = N.Fs.Make(val fs) in
        let src' = N.Path.join [src.T.File_stat.directory;src.T.File_stat.filename] in
        let new_name = N.Path.join [src.T.File_stat.directory; new_name] in

        if new_name = src' then
          Lwt.return @@ T.Task_result.(Ok Payload_rename)
        else match Fs.renameSync src' new_name with
          | Ok _ -> Lwt.return @@ T.Task_result.(Ok Payload_rename)
          | Error _ -> Lwt.return @@ T.Task_result.(of_error "")
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
