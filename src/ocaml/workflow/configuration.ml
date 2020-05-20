open Abbrev

type events = Updated of D.Configuration_store.t [@@deriving eq, show]

module Update = struct
  type input = {
    key : D.Configuration_store.Key.t;
    value : Yojson.Basic.t;
  }

  type work_flow = input -> events list Lwt.t
  (** workflow to add a key binding for action to key map *)
end

type commands = Update of Update.input

let update : Common_step_configuration.load -> Common_step_configuration.save -> Update.work_flow =
 fun load save { key; value } ->
  let%lwt store = load () in
  let store' = D.Configuration_store.put ~key ~value store in
  let%lwt () = save store' in
  Lwt.return [ Updated store' ]
