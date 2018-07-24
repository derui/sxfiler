(** {!Completion} provides status of completion for renderer.  *)
module T = Sxfiler_types
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t
  type t = {
    current_class: T.Completion.Source_class.t;
    candidates: T.Completion.result;
  }

  let make () = {
    current_class = T.Completion.Source_class.Simple;
    candidates = [||];
  }

  let reduce t = function
    | C.Message.Completion (Setup cls) -> {current_class = cls; candidates = [||]}
    | Completion (Read result) -> {t with candidates = result}
    | _ -> t

  let equal = (=)
end

module Store = C.Store.Make(State)
