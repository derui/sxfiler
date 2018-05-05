module T = Common_types

module Dialog_state = struct
  type t =
      Open of T.Dialog_type.t
    | Close
end

type t = {
  server_state: Server_state.t;
  dialog_state: Dialog_state.t;
  completer_state: Completer_state.t;
}

let empty = {
  server_state = Server_state.empty;
  dialog_state = Dialog_state.Close;
  completer_state = Completer_state.empty;
}
