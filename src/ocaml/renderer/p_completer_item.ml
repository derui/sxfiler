(** {!P_completer_item} defines presenter component to show a candidate. *)

module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless(struct
    class type t = object
      method candidate: T.Completion.Candidate.t Js.readonly_prop
    end
  end)

let t =
  Component.make (fun props ->
      let candidate = props##.candidate in
      let module C = T.Completion.Candidate in
      let module I = T.Completion.Item in

      [%e li ~class_name:"sf-CompleterItem" [candidate.C.value.I.value [@txt]]]
    )
