(** {!P_completer_item} defines presenter component to show a candidate. *)

module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store
module T = Sxfiler_rpc.Types

let t = R.Component.make_stateless
    ~props:(module struct
             class type t = object
               method candidate: T.Completion.Candidate.t Js.readonly_prop
               method selected: bool Js.t Js.readonly_prop
             end
           end)

    ~render:(fun props ->
        let candidate = props##.candidate in
        let module C = T.Completion.Candidate in
        let module I = T.Completion.Item in
        let class_name = Classnames.to_string [
            "sf-CompleterItem", true;
            "sf-CompleterItem-selected", Js.to_bool props##.selected;
          ]
        in

        [%e li ~class_name [candidate.C.value.I.value [@txt]]]
      )
