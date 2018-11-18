(** {!Command} provides status of command to manage command execution on render-er.  *)
module T = Sxfiler_rpc.Types

module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t

  type t =
    { current_command : string option
    ; plan : T.Plan.t option
    ; corrections : C.Types.corrections
    ; planning : bool }

  let make () = {current_command = None; plan = None; planning = false; corrections = []}

  let reduce t = function
    | C.Message.Command Planning -> {t with planning = true}
    | C.Message.Command (Plan plan) -> {t with planning = false; plan = Some plan}
    | C.Message.Command (Edit correction) -> {t with corrections = correction :: t.corrections}
    | C.Message.Command Finished | C.Message.Command Reject ->
      {t with corrections = []; plan = None; current_command = None}
    | _ -> t

  let equal = ( = )
end

module Store = C.Store.Make (State)
