module C = Sxfiler_common
module M = C.Message

type message = M.t
type key_action = C.Callable_action.t
type action = Context.t -> C.State.t Lwt.t

(** Create action that is executable with renderer context *)
let create = function
  | C.Callable_action.Core action -> begin
      let module Core = C.Callable_action.Core in
      match action with
      | Core.Jump -> fun ctx -> Lwt.return @@ {ctx.Context.state with
                                               dialog_state = let module D = C.State.Dialog_state in
                                                 D.Open C.Types.Dialog_type.Jump
                                              }
      | Unknown action -> fun ctx -> Lwt.fail_with @@ Printf.sprintf "Unknown action for core module: %s" action
      | _ -> failwith "not implemented yet"
    end
  | _ -> fun _ -> Lwt.fail_with "Not implemented yet for modules excluded Core module."

let create_from_message = function
  | M.Close_dialog -> fun ctx -> Lwt.return @@ {ctx.Context.state with
                                                dialog_state = C.State.Dialog_state.Close}
  | _ -> failwith "not implemented yet"
