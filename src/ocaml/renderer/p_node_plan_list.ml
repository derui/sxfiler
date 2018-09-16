(** This component displays list of items that is planned by some command. *)

module R = Jsoo_reactjs
module T = Sxfiler_rpc.Types

let header txt =
  [%e header ~key:"header" ~class_name:"fp-NodePlanList_Header" [(Js.to_string txt [@txt])]]

let content plans =
  let to_item node_plan =
    let key = node_plan.T.Plan.node.T.Node.id in
    [%c
      P_node_plan_item.t ~key
        ~props:
          (object%js
            val nodePlan = node_plan
          end)]
  in
  let children = List.map to_item plans in
  [%e ul ~key:"content" ~class_name:"fp-NodePlanList_Content" children]

let t =
  R.Component.make_stateless
    ~props:
      ( module struct
        class type t =
          object
            method nodePlans : T.Plan.node_plan list Js.readonly_prop

            method name : Js.js_string Js.t Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        let plans = props##.nodePlans in
        [%e div ~class_name:"fp-NodePlanList" [header props##.name; content plans]] )
