(** {!Completion} provides status of completion for renderer.  *)
module T = Sxfiler_types
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t

  type t = {
    current_class: T.Completion.Source_class.t;
    candidates: T.Completion.result;
    selected_id: string;
    current_completer: string option;
  }

  let make () = {
    current_class = T.Completion.Source_class.Simple;
    candidates = [||];
    selected_id = "";
    current_completer = None;
  }

  let reduce t = function
    | C.Message.Completion (Setup (cls, completer)) -> {current_class = cls;
                                                        candidates = [||];
                                                        selected_id = "";
                                                        current_completer = Some completer;
                                                       }
    | Completion (Read result) -> {t with candidates = result}
    | Completion Tear_down -> {t with current_completer = None}
    | _ -> t

  let equal = (=)
end

module Store = C.Store.Make(State)
