open Sxfiler_renderer_core
open Store

module Viewer_stacks_store = Make(State.Viewer_stacks)
let viewer_stacks : Viewer_stacks_store.t Tag.def = Tag.def ~name:"viewer_stacks"

module Config_store = Make(State.Config)
let config : Config_store.t Tag.def = Tag.def ~name:"config"

module Layout_store = Make(State.Layout)
let layout : Layout_store.t Tag.def = Tag.def ~name:"layout"
