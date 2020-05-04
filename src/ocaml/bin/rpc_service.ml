module R = Sxfiler_rpc
module G = Sxfiler_generated

let construct_filer_endpoints (module Dep : Dependencies.S) =
  [
    (G.Service.Command.FILER_INITIALIZE, R.Filer_endpoint.initialize Dep.Work_flow.Filer.initialize);
    (G.Service.Command.FILER_RELOAD_ALL, R.Filer_endpoint.reload_all Global.Filer.get Dep.Work_flow.Filer.reload_all);
    ( G.Service.Command.FILER_MOVE_LOCATION,
      R.Filer_endpoint.move_location Global.Filer.get Dep.Work_flow.Filer.move_location );
    (G.Service.Command.FILER_OPEN_FILE_ITEM, R.Filer_endpoint.open_node Global.Filer.get Dep.Work_flow.Filer.open_node);
    ( G.Service.Command.FILER_UP_DIRECTORY,
      R.Filer_endpoint.up_directory Global.Filer.get Dep.Work_flow.Filer.up_directory );
    ( G.Service.Command.FILER_TOGGLE_MARK_OF_ITEM,
      R.Filer_endpoint.toggle_mark Global.Filer.get Dep.Work_flow.Filer.toggle_mark );
    (G.Service.Command.FILER_MOVE, R.Filer_endpoint.move Global.Filer.get Dep.Work_flow.Filer.move);
    (G.Service.Command.FILER_COPY, R.Filer_endpoint.copy Global.Filer.get Dep.Work_flow.Filer.copy);
    (G.Service.Command.FILER_DELETE, R.Filer_endpoint.delete Global.Filer.get Dep.Work_flow.Filer.delete);
  ]

let construct_keymap_endpoints (module Dep : Dependencies.S) options =
  [
    (G.Service.Command.KEYMAP_ADD_KEY_BINDING, R.Keymap_endpoint.add_key_binding Dep.Work_flow.Keymap.add_key_binding);
    ( G.Service.Command.KEYMAP_REMOVE_KEY_BINDING,
      R.Keymap_endpoint.remove_key_binding Dep.Work_flow.Keymap.remove_key_binding );
    (* query endpoint *)
    (G.Service.Command.KEYMAP_GET, R.Keymap_endpoint.get Dep.Step.resolve_keymap);
    ( G.Service.Command.KEYMAP_RELOAD,
      R.Keymap_endpoint.reload options.App_option.keymap_file Dep.Work_flow.Keymap.reload );
  ]

let construct_configuration_endpoints (module Dep : Dependencies.S) =
  [ (G.Service.Command.CONFIGURATION_GET, R.Configuration_endpoint.get Dep.Step.load_configuration) ]

let construct_completer_endpoints (module Dep : Dependencies.S) =
  [
    (G.Service.Command.COMPLETER_INITIALIZE, R.Completer_endpoint.initialize Dep.Work_flow.Completer.initialize);
    (G.Service.Command.COMPLETER_COMPLETE, R.Completer_endpoint.complete Dep.Work_flow.Completer.complete);
  ]

let construct_services (module Dep : Dependencies.S) options =
  List.concat
    [
      construct_filer_endpoints (module Dep);
      construct_keymap_endpoints (module Dep) options;
      construct_configuration_endpoints (module Dep);
      construct_completer_endpoints (module Dep);
    ]
