(** {!P_completer_item} defines presenter component to show a candidate. *)

module T = Sxfiler_completion.Domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let t = R.Component.make_stateless
    ~props:(module struct
             class type t = object
               method candidate: T.Candidate.t Js.readonly_prop
             end
           end)

    ~render:(fun props ->
        let candidate = props##.candidate in
        let module C = T.Candidate in
        let module I = T.Item in

        [%e li ~class_name:"sf-CompleterItem" [candidate.C.value.I.value [@txt]]]
      )
