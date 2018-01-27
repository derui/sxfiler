module C = Sxfiler_common
module R = Reactjscaml
module M = Sxfiler_modules

let container_id = "top-entry"

let () =
  let electron = M.electron in
  let container = Dom_html.getElementById container_id in
  let dispatcher = Sxfiler_key_dispatcher.make electron##.ipcRenderer in

  let handle _ = function
    | `Update t -> begin
        let element = R.element ~props:(object%js
            val dispatch = dispatcher
            val state = t
          end) Sxfiler_key_container.component in
        R.dom##render element container
      end
    | _ -> ()
  in

  C.Event.IPC.(on ~target:Listener.update ~f:handle electron##.ipcRenderer)
