(** {!C_completer} provides container component that will show candidates of completion and
    select it.

    This component will appear beside of a base component that is passed from props.
*)

module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

let t =
  R.Component.make_stateless
    ~props:
      ( module struct
        class type t =
          object
            method completerId : string Js.readonly_prop

            method completion : S.Completion.State.t Js.readonly_prop

            method showed : bool Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        let module S = S.Completion.State in
        let completion = props##.completion in
        if (not props##.showed) || List.length completion.S.candidates = 0 then R.empty ()
        else
          let children =
            List.map
              (fun candidate ->
                 let module C = T.Completion in
                 [%c
                   P_completer_item.t ~key:candidate.C.Candidate.value.C.Item.id
                     ~props:
                       (object%js
                         val candidate = candidate

                         val selected = Js.bool (candidate.value.C.Item.id = completion.S.selected_id)
                       end)] )
              completion.S.candidates
          in
          [%e ul ~class_name:"sf-Completer" children] )
