module C = Sxfiler_common.Std
module R = Reactjscaml.Std
module Runner = Sxfiler_flux_runner
module Ipc = Sxfiler_renderer_ipc

let container_id = "top-entry"

let () =
  let container = Dom_html.getElementById container_id in

  let runner = Sxfiler_flux_runner.run ~initial_state:(Sxfiler_state.empty ()) () in
  let dispatcher = Sxfiler_connector.connect runner in
  let module Renderer = struct
    let handle t =
      let element = R.element ~props:(object%js
          val dispatch = dispatcher
          val state = Sxfiler_state.to_js t
        end) Sxfiler_file_list.component in
      Lwt.return @@ R.dom##render element container
  end in

  Sxfiler_flux_runner.subscribe ~subscription:(module Renderer) runner
