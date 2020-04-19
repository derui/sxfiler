open Abbrev

type events = Updated of D.Configuration.t [@@deriving eq, show]

module Update = struct
  type input = D.Configuration.t

  type work_flow = input -> events list Lwt.t
  (** workflow to add a key binding for action to key map *)
end

type commands = Update of Update.input

let update : Common_step_configuration.save -> Update.work_flow =
 fun save input ->
  let%lwt () = save input in
  Lwt.return [ Updated input ]
