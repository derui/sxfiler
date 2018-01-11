module C = Sxfiler_common.Std
module R = Reactjscaml.Std
module Runner = Sxfiler_flux_runner
module Ipc = Sxfiler_renderer_ipc

let container_id = "top-entry"

let () =
  let dispatcher, adapter = Sxfiler_action.make () in
  let container = Dom_html.getElementById container_id in

  let module Renderer = struct
    let handle t =
      let element = R.element ~props:(object%js
          val dispatch = dispatcher
          val state = Sxfiler_state.to_js t
        end) Sxfiler_file_list.component in
      Lwt.return @@ R.dom##render element container
  end in

  let runner = Sxfiler_flux_runner.run ~subscriptions:[(module Renderer)]
      ~initial_state:(Sxfiler_state.empty ()) in
  Sxfiler_action.connect ~runner adapter;
