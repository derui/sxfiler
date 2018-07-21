module T = Sxfiler_types
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = {
    config: T.Configuration.t;
    mode: C.Types.Mode.t;
    condition: C.Types.Condition.t;
  }

  let make () = {
    config = T.Configuration.default;
    mode = C.Types.Mode.File_tree;
    condition = {C.Types.Condition.on_file_tree = true};
  }

  let reduce t = function
    | C.Message.Switch_mode mode -> begin match mode with
        | C.Types.Mode.File_tree -> {t with mode = mode;
                                            condition = {C.Types.Condition.on_file_tree = true}
                                    }
        | _ -> t
      end
    | _ -> t

  let equal = (=)

  let condition {condition;_} = condition
end

module Store = C.Store.Make(State)
