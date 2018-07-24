(** {!Completion} provides status of completion for renderer.  *)
module T = Sxfiler_types
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = {
    current_class: T.Completion.Source_class.t;
    candidates: T.Completion.result;
    completing: bool;
  }

  let make () = {
    current_class = T.Completion.Source_class.Simple;
    candidates = [||];
    completing = false;
  }

  let reduce t = function
    | C.Message.Completion (Setup cls) -> {t with current_class = cls; completing = true}
    | Completion (Read result) -> {t with candidates = result}
    | Completion Tear_down -> {t with completing = false}
    | _ -> t

  let equal = (=)
end

module Store = C.Store.Make(State)
