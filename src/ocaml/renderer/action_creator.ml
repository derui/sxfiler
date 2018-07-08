module T = Sxfiler_types_jsoo
module M = T.Message

type message = M.t
type key_action = T.Callable_action.t
type action = Context.t -> State.t Lwt.t

(** Create action that is executable with renderer context *)
let create = function
  | T.Callable_action.Core action -> begin
      let module Core = T.Callable_action.Core in
      match action with
      | _ -> failwith "not implemented yet"
    end
  | _ -> fun _ -> Lwt.fail_with "Not implemented yet for modules excluded Core module."

let rec create_from_message = function
  | _ -> failwith "not implemented yet"
