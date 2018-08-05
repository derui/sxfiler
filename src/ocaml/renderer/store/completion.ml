(** {!Completion} provides status of completion for renderer.  *)
module C = Sxfiler_renderer_core
module Co = Sxfiler_completion.Domain

module State = struct
  type message = C.Message.t

  type t = {
    candidates: Co.result;
    selected_id: string;
    current_completer: string option;
  }

  let make () = {
    candidates = [];
    selected_id = "";
    current_completer = None;
  }

  let reduce t = function
    | C.Message.Completion (Setup completer) -> {
        candidates = [];
        selected_id = "";
        current_completer = Some completer;
      }
    | Completion (Read result) -> {t with candidates = result}
    | Completion Tear_down -> {t with current_completer = None}
    | _ -> t

  let equal = (=)
end

module Store = C.Store.Make(State)
