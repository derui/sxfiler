module C = Sxfiler_common.Std
module R = Reactjscaml.Std
module M = Sxfiler_modules

let container_id = "top-entry"

let () =
  let electron = M.electron in
  let container = Dom_html.getElementById container_id in
  let dispatcher = Sxfiler_dispatcher.make electron##.ipcRenderer in

  let handle _ t =
    let element = R.element ~props:(object%js
        val dispatch = dispatcher
        val state = C.State.to_js t
      end) Sxfiler_file_list.component in
    R.dom##render element container
  in
  C.Event.IPC.(on ~target:Listener.update ~f:handle electron##.ipcRenderer);
