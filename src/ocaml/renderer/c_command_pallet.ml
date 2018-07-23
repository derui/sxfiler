(** {!C_command_pallet} defines container component to be used to input and execute commands. *)

module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateful(struct
    class type t = object
      method context: (module Context.Instance) Js.readonly_prop
    end
  end)(struct
    type t = C.Command.t
  end)
