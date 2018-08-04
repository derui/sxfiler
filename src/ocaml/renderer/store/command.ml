(** {!Command} provides status of command to manage command execution on render-er.  *)
module T = Sxfiler_domain
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t

  type t = {
    current_command: string option;
    planning: bool;
    preparing: bool;
  }

  let make () = {
    current_command = None;
    planning = false;
    preparing = false;
  }

  let reduce t = function
    | _ -> t

  let equal = (=)
end

module Store = C.Store.Make(State)
