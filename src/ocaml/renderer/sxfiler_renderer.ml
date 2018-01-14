module C = Sxfiler_common.Std
module R = Reactjscaml.Std
module M = Sxfiler_modules

let container_id = "top-entry"

let () =
  let electron = M.electron in
  let container = Dom_html.getElementById container_id in
  let dispatcher = Sxfiler_dispatcher.make electron##.ipcRenderer in

  let handle _ = function
    | `Update t -> begin
        let element = R.element ~props:(object%js
            val dispatch = dispatcher
            val state = t
          end) Sxfiler_file_list.component in
        R.dom##render element container
      end
    | _ -> ()
  in
  C.Event.IPC.(on ~target:Listener.update ~f:handle electron##.ipcRenderer);

  let module M = Sxfiler_common.Std.Message in
  Sxfiler_dispatcher.dispatch dispatcher @@ M.request_files_in_directory "/home/derui"
