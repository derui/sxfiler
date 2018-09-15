module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module L = Modules.Lodash

let t =
  R.Component.make_stateful
    ~props:
      ( module struct
        class type t =
          object
            method nodePlan : T.Plan.node_plan Js.readonly_prop
          end
      end )
    ~spec:
      (R.component_spec
         ~initial_state:(fun _ _ -> object%js end)
         ~initial_custom:(fun _ _ -> object%js end)
         ~should_component_update:(fun this new_props _ -> not @@ L.isEqual this##.props new_props)
         (fun this ->
            let node_plan = this##.props##.nodePlan in
            let stat = node_plan.T.Plan.node.T.Node.stat and node = node_plan.node in
            let module F = T.File_stat in
            let file_mode =
              [%c
                P_file_mode.t ~key:"mode"
                  ~props:
                    (object%js
                      val mode = Int32.of_string stat.F.mode

                      val className = Js.string "fp-NodePlanItem_FileMode"
                    end)]
            and file_size =
              [%c
                P_file_size.t ~key:"size"
                  ~props:
                    (object%js
                      val size = Int64.of_string stat.F.size

                      val className = Js.string "fp-NodePlanItem_FileSize"
                    end)]
            and file_name =
              [%c
                P_file_name.t ~key:"name"
                  ~props:
                    (object%js
                      val name = node.T.Node.name

                      val isDirectory = stat.F.is_directory

                      val isSymbolicLink = stat.F.is_symlink

                      val className = Js.string "fp-NodePlanItem_FileName"
                    end)]
            and operation =
              [%c
                P_node_plan_operation.t ~key:"operation"
                  ~props:
                    (object%js
                      val operation = node_plan.operation
                    end)]
            in
            let class_name = Classnames.to_string [("fp-NodePlanItem", true)] in
            [%e li ~class_name [operation; file_mode; file_size; file_name]] ))
