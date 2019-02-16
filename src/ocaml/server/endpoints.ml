(** This module provides endpoints for RPC methods. *)

(** endpoints for Completion *)
module Completion = struct
  let setup = "completion/setup"
  let read = "completion/read"
end

(** endpoints for Filer *)
module Filer = struct
  let make = "filer/make"
  let get = "filer/get"
  let move_parent = "filer/moveParent"
  let enter_directory = "filer/enterDirectory"
end

(** endpoints for Configuration *)
module Configuration = struct
  let get = "configuration/get"
end

(** endpoint for plan *)
module Plan = struct
  let reject = "plan/reject"

  module Filer = struct
    let move_nodes = "plan/filer/moveNodes"
    let delete_nodes = "plan/filer/deleteNodes"
  end
end

module Notification = struct
  let notify = "notification/message"
  let progress = "notification/progress"
end
