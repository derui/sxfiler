module T = Sxfiler_rpc.Types
module R = Jsoo_reactjs
module L = Modules.Lodash

let t =
  R.Component.make_stateful
    ~props:
      ( module struct
        class type t =
          object
            method item : T.Node.t Js.readonly_prop

            method marked : bool Js.readonly_prop

            method selected : bool Js.readonly_prop
          end
      end )
    ~spec:
      (R.component_spec
         ~initial_state:(fun _ _ -> object%js end)
         ~initial_custom:(fun _ _ -> object%js end)
         ~should_component_update:(fun this new_props _ -> not @@ L.isEqual this##.props new_props)
         (fun this ->
            let props = this##.props in
            let node = props##.item in
            let stat = node.T.Node.stat in
            let module F = T.File_stat in
            let file_mode =
              [%c
                P_file_mode.t ~key:"mode"
                  ~props:
                    (object%js
                      val mode = Int32.of_string stat.F.mode
                    end)]
            and timestamp =
              [%c
                P_file_timestamp.t ~key:"timestamp"
                  ~props:
                    (object%js
                      val timestamp = Int64.of_string stat.F.mtime |> Int64.to_float
                    end)]
            and file_size =
              [%c
                P_file_size.t ~key:"size"
                  ~props:
                    (object%js
                      val size = Int64.of_string stat.F.size
                    end)]
            and file_name =
              [%c
                P_file_name.t ~key:"name"
                  ~props:
                    (object%js
                      val name = node.T.Node.name

                      val isDirectory = stat.F.is_directory

                      val isSymbolicLink = stat.F.is_symlink
                    end)]
            in
            let class_name =
              Classnames.to_string
                [ ("fp-FileItem", true)
                ; ("fp-FileItem-selected", props##.selected)
                ; ("fp-FileItem-marked", props##.marked) ]
            in
            [%e li ~class_name [file_mode; timestamp; file_size; file_name]] ))
