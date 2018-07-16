module T = Sxfiler_types
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = {
    config: Config.Store.t;
    layout: Layout.Store.t;
    viewer_stacks: Viewer_stacks.Store.t
  }

  let make ~config ~layout ~viewer_stacks = {
    config;
    layout;
    viewer_stacks;
  }

  let reduce t message =
    Config.Store.dispatch t.config message;
    Layout.Store.dispatch t.layout message;
    Viewer_stacks.Store.dispatch t.viewer_stacks message;
    t

  let config {config;_} = config
  let layout {layout;_} = layout
  let viewer_stacks {viewer_stacks;_} = viewer_stacks

  let equal v1 v2 =
    Config.(State.equal (Store.get v1.config) (Store.get v2.config))
    && Layout.(State.equal (Store.get v1.layout) (Store.get v2.layout))
    && Viewer_stacks.(State.equal (Store.get v1.viewer_stacks) (Store.get v2.viewer_stacks))

end

module Store = C.Store.Make_group(State)(struct
    type state = State.t

    let watch_state callback state =
      Config.Store.subscribe state.State.config
        ~f:(fun _ -> callback state);
      Layout.Store.subscribe state.State.layout
        ~f:(fun _ -> callback state);
      Viewer_stacks.Store.subscribe state.State.viewer_stacks
        ~f:(fun _ -> callback state)
  end)
