(** This component defines function how to display operation. *)

module D = Sxfiler_domain
module R = Jsoo_reactjs

let t =
  R.Component.make_stateless
    ~props:
      ( module struct
        class type t =
          object
            method operation : D.Plan.Operation.t Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        let op = props##.operation in
        let text =
          match op with Delete -> "-" | Remained -> "=" | Append -> "+" | Conflict -> "|"
        in
        let class_name = Classnames.to_string [("fp-NodePlanItem_Operation", true)] in
        [%e span ~class_name [(text [@txt])]] )
