open Sxfiler_renderer_core

module Viewer_stacks_store = Store.Make(State.Viewer_stacks)
let viewer_stacks : (Viewer_stacks_store.t, Message.t) Tag.def =
  Tag.def ~name:"viewer_stacks" ~store:(module Viewer_stacks_store)

module Config_store = Store.Make(State.Config)
let config : (Config_store.t, Message.t) Tag.def =
  Tag.def ~name:"config" ~store:(module Config_store)

module Layout_store = Store.Make(State.Layout)
let layout : (Layout_store.t, Message.t) Tag.def =
  Tag.def ~name:"layout" ~store:(module Layout_store)
