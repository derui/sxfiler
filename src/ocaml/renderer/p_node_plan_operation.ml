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
        let class_name =
          Classnames.to_string
            [ ("fp-NodePlanItem_Operation", true)
            ; ("fp-NodePlanItem_Operation-deleted", op = D.Plan.Operation.Delete)
            ; ("fp-NodePlanItem_Operation-remained", op = D.Plan.Operation.Remained)
            ; ("fp-NodePlanItem_Operation-appended", op = Append)
            ; ("fp-NodePlanItem_Operation-conflicted", op = Conflict) ]
        in
        [%e span ~class_name []] )
