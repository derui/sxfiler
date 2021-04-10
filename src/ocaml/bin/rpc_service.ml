module R = Sxfiler_rpc
module D = Sxfiler_domain
module G = Sxfiler_generated
module F = Sxfiler_workflow
module S = Sxfiler_dependency

let construct_filer_endpoints deps =
  [
    (G.Service.Command.FILER_INITIALIZE, R.Filer_endpoint.initialize F.Filer.initialize deps);
    (G.Service.Command.FILER_RELOAD_ALL, R.Filer_endpoint.reload_all F.Filer.reload_all deps);
    (G.Service.Command.FILER_MOVE_LOCATION, R.Filer_endpoint.move_location F.Filer.move_location deps);
    (G.Service.Command.FILER_OPEN_FILE_ITEM, R.Filer_endpoint.open_node F.Filer.open_node deps);
    (G.Service.Command.FILER_UP_DIRECTORY, R.Filer_endpoint.up_directory F.Filer.up_directory deps);
    (G.Service.Command.FILER_TOGGLE_MARK_OF_ITEM, R.Filer_endpoint.toggle_mark F.Filer.toggle_mark deps);
    (G.Service.Command.FILER_MOVE, R.Filer_endpoint.move F.Filer.move deps);
    (G.Service.Command.FILER_COPY, R.Filer_endpoint.copy F.Filer.copy deps);
    (G.Service.Command.FILER_DELETE, R.Filer_endpoint.delete F.Filer.delete deps);
  ]

let construct_keymap_endpoints deps options =
  [
    (G.Service.Command.KEYMAP_ADD_KEY_BINDING, R.Keymap_endpoint.add_key_binding F.Keymap.add_key_binding deps);
    (G.Service.Command.KEYMAP_REMOVE_KEY_BINDING, R.Keymap_endpoint.remove_key_binding F.Keymap.remove_key_binding deps);
    (* query endpoint *)
    (G.Service.Command.KEYMAP_GET, R.Keymap_endpoint.get deps);
    (G.Service.Command.KEYMAP_RELOAD, R.Keymap_endpoint.reload options.App_option.keymap_file F.Keymap.reload deps);
  ]

let construct_configuration_endpoints deps =
  [
    (G.Service.Command.CONFIGURATION_GET, R.Configuration_endpoint.get deps);
    (G.Service.Command.CONFIGURATION_UPDATE, R.Configuration_endpoint.update F.Configuration.update deps);
  ]

let construct_completer_endpoints deps =
  [
    (G.Service.Command.COMPLETER_INITIALIZE, R.Completer_endpoint.initialize deps);
    (G.Service.Command.COMPLETER_COMPLETE, R.Completer_endpoint.complete deps);
  ]

let construct_theme_endpoints deps =
  [
    (G.Service.Command.THEME_GET, R.Theme_endpoint.get deps);
    (G.Service.Command.THEME_UPDATE, R.Theme_endpoint.update deps);
  ]

let construct_services (module Dep : Dependencies.S) options =
  let deps = function
    | `Step_theme_instance c         -> S.Context.value (module Dep.Step.Theme : F.Common_step.Theme.Instance) c
    | `Step_file_list_instance c     -> S.Context.value (module Dep.Step.File_list : F.Common_step.File_list.Instance) c
    | `Step_filer_instance c         -> S.Context.value (module Dep.Step.Filer : F.Common_step.Filer.Instance) c
    | `Step_keymap_instance c        -> S.Context.value (module Dep.Step.Keymap : F.Common_step.Keymap.Instance) c
    | `Step_common_instance c        -> S.Context.value (module Dep.Step.Common : F.Common_step.Instance) c
    | `Step_interaction_instance c   ->
        S.Context.value (module Dep.Step.Interaction : F.Common_step.Interaction.Instance) c
    | `Step_configuration_instance c ->
        S.Context.value (module Dep.Step.Configuration : F.Common_step.Configuration.Instance) c
    | `Completer_instance c          -> S.Context.value (module Dep.Completer : D.Completer.Instance) c
    | `Step_completer_instance c     -> S.Context.value (module Dep.Step.Completer : F.Common_step.Completer.Instance) c
  in
  List.concat
    [
      construct_filer_endpoints deps;
      construct_keymap_endpoints deps options;
      construct_configuration_endpoints deps;
      construct_completer_endpoints deps;
      construct_theme_endpoints deps;
    ]
