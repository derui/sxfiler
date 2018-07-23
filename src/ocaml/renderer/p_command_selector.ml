(** {!P_command_selector} defines presentation component to select command. *)

module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless(struct
    class type t = object
      method context: (module Context.Instance) Js.readonly_prop
    end
  end)
