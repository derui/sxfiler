(** {!C_completion_result} defines container component to present candidates that are
    result of completion.
*)

open Sxfiler_core
module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless(struct
    class type t = object
      method context: (module Context.Instance) Js.readonly_prop
    end
  end)

let component = Component.make (fun props ->

    R.empty ()
  )
