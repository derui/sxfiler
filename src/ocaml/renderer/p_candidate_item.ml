(** {!P_candidate_item} defines presentation component to display a candidate in
    completion list.
*)

module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless(struct
    class type t = object
      method candidate: T.Completion.Candidate.t Js.readonly_prop
    end
  end)

let component = Component.make (fun props ->
    let item = props##.candidate in
    let class_name = Classnames.to_string [
        ("fp-CandidateItem", true)
      ]
    in
    let props = R.element_spec () ~class_name in
    let text = T.Completion.(item.Candidate.value.Item.value) in

    R.Dom.of_tag `li ~props
      ~children:[
        R.Dom.of_tag `div ~props:R.(element_spec ~class_name:"fp-CandidateItem_Content" ())
          ~children:[R.text text]
      ]
  )
