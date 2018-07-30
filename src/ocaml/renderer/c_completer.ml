(** {!C_completer} provides container component that will show candidates of completion and
    select it.

    This component will appear beside of a base component that is passed from props.
*)

module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless(struct
    class type t = object
      method completerId: string Js.readonly_prop
      method completion: S.Completion.State.t Js.readonly_prop
      method showed: bool Js.readonly_prop
    end
  end)

let t = Component.make (fun props ->
    if not props##.showed then R.empty ()
    else
      let completion = props##.completion in
      let module S = S.Completion.State in
      let spec = R.element_spec ~class_name:"sf-Completer" () in
      let children = Array.to_list @@ Array.map (fun candidate ->
          let open T.Completion in
          R.create_element ~key:Candidate.(id candidate)
            ~props:(object%js
              val candidate = candidate
            end)
            P_completer_item.t
        ) completion.S.candidates
      in
      R.Dom.of_tag `ul
        ~props:spec
        ~children
  )
