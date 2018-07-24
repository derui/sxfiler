(** {!C_candidate_display} defines container component to present candidates that are
    result of completion.
*)

module T = Sxfiler_types
module R = Jsoo_reactjs
module C = Sxfiler_renderer_core
module S = Sxfiler_renderer_store

module Component = R.Component.Make_stateless(struct
    class type t = object
      method candidates: T.Completion.result Js.readonly_prop
    end
  end)

let component = Component.make (fun props ->
    let candidates = props##.candidates in
    let props = R.element_spec () ~class_name:"fp-CandidateDisplay" in
    let to_item candidate =
      let key = T.Completion.(candidate.Candidate.value.Item.id) in
      R.create_element ~key ~props:(object%js
        val candidate = candidate
      end)
        P_candidate_item.component
    in

    R.Dom.of_tag `ul ~key:"completion-result" ~props
      ~children:(Array.to_list @@ Array.map to_item candidates)
  )
