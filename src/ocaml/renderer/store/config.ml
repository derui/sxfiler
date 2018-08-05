module T = Sxfiler_domain
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = {
    config: T.Configuration.t;
    condition: T.Condition.t;
  }

  let make () = {
    config = T.Configuration.default;
    condition = T.Condition.(of_list [On_file_tree]);
  }

  let reduce t = function
    | C.Message.Switch_mode mode -> begin match mode with
        | C.Types.Mode.File_tree -> {t with condition = T.Condition.(enable ~context:On_file_tree t.condition)}
      end
    | _ -> t

  let equal = (==)

  let condition {condition;_} = condition

  let is_subset t ~cond = T.Condition.subset ~current:t.condition ~parts:cond
end

module Store = C.Store.Make(State)
