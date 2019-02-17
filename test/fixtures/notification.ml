module D = Sxfiler_domain

let state_for_fixture = Random.State.make_self_init ()

(* make fixture for node *)
let for_message ?(level = D.Notification.Level.Info) message =
  let id = Uuidm.(v4_gen state_for_fixture ()) in
  D.Notification.make ~id ~level ~body:(D.Notification.Message message)

let for_progress ?(level = D.Notification.Level.Info) ?(process = "fixture") ~current ~targeted ()
  =
  let id = Uuidm.(v4_gen state_for_fixture ()) in
  D.Notification.make ~id ~level ~body:(D.Notification.Progress {process; current; targeted})
