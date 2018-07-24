module T = Sxfiler_types
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = {
    config: T.Configuration.t;
    condition: C.Types.Condition.t;
  }

  let make () = {
    config = T.Configuration.default;
    condition = C.Types.Condition.(of_list [On_file_tree]);
  }

  let reduce t = function
    | C.Message.Switch_mode mode -> begin match mode with
        | C.Types.Mode.File_tree -> {t with condition = C.Types.Condition.(enable ~context:On_file_tree t.condition)}
        | C.Types.Mode.Completion -> {t with condition = C.Types.Condition.(enable ~context:On_file_tree t.condition)}
        | _ -> t
      end
    | _ -> t

  let equal = (==)

  let condition {condition;_} = condition

  let is_subset t ~cond = C.Types.Condition.subset ~current:t.condition ~parts:cond
end

module Store = C.Store.Make(State)
