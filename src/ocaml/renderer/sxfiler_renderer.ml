module C = Sxfiler_common.Std
module R = Reactjscaml.Std
module Runner = Sxfiler_flux_runner
module Ipc = Sxfiler_renderer_ipc

let container_id = "top-entry"

let () =
  let element = R.element ~props:(object%js
      val files = Js.array [||]
    end) Sxfiler_file_list.component in
  let container = Dom_html.getElementById container_id in
  R.dom##render element container
