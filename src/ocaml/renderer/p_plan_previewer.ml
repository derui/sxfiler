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
            method plan : T.Plan.t Js.readonly_prop
          end
      end )
    ~render:(fun props ->
        let state = props##.plan in
        let source = state.source and dest = state.dest in
        [%e
          div ~class_name:"fp-PlanPreviewer"
            [ [%c
              P_node_plan_list.t ~key:"source"
                ~props:
                  (object%js
                    val nodePlans = source
                  end)]
            ; [%c
              P_node_plan_list.t ~key:"dest"
                ~props:
                  (object%js
                    val nodePlans = dest
                  end)] ]] )
