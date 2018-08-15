(** {!Completion} provides status of completion for renderer.  *)
module C = Sxfiler_renderer_core
module Co = Sxfiler_completion.Domain

module State = struct
  type message = C.Message.t

  type t = {
    candidates: Co.result;
    selected_index: int;
    selected_id: string;
    current_completer: string option;
  }

  let make () = {
    candidates = [];
    selected_index = 0;
    selected_id = "";
    current_completer = None;
  }

  let reduce t = function
    | C.Message.Completion (Setup completer) -> {
        candidates = [];
        selected_id = "";
        selected_index = 0;
        current_completer = Some completer;
      }
    | Completion (Read result) -> {t with candidates = result}
    | Completion Tear_down -> {t with current_completer = None}
    | Completion Select_next -> begin
        let new_index = max 0 (min (List.length t.candidates - 1) (succ t.selected_index)) in
        {t with selected_index = new_index;
                selected_id = match List.nth_opt t.candidates t.selected_index with
                  | None -> ""
                  | Some v -> v.Co.Candidate.value.Co.Item.value
        }
        end
    | Completion Select_prev -> begin
        let new_index = max 0 (min (List.length t.candidates - 1) (pred t.selected_index)) in
        {t with selected_index = new_index;
                selected_id = match List.nth_opt t.candidates t.selected_index with
                  | None -> ""
                  | Some v -> v.Co.Candidate.value.Co.Item.value
        }
        end
    | _ -> t

  let equal = (=)

  (** [selected_item t] gets current selected item. *)
  let selected_item t =
    List.find_opt (fun v -> v.Co.Candidate.value.Co.Item.id = t.selected_id)
      t.candidates
end

module Store = C.Store.Make(State)
