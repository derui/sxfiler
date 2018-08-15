(** {!C_completer} provides container component that will show candidates of completion and
    select it.

    This component will appear beside of a base component that is passed from props.
*)

module T = Sxfiler_completion.Domain
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let t = R.Component.make_stateless
    ~props:(module struct
             class type t = object
               method completerId: string Js.readonly_prop
               method completion: S.Completion.State.t Js.readonly_prop
               method showed: bool Js.readonly_prop
             end
           end)
    ~render:(fun props ->
        if not props##.showed then R.empty ()
        else
          let completion = props##.completion in
          let module S = S.Completion.State in
          let children = List.map (fun candidate ->
              let open T in
              [%c P_completer_item.t ~key:Candidate.(id candidate) ~props:(object%js
                  val candidate = candidate
                  val selected = Js.bool (candidate.value.Item.id = completion.S.selected_id)
                end)]
            ) completion.S.candidates
          in
          [%e ul ~class_name:"sf-Completer" children]
      )
