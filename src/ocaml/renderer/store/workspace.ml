module T = Sxfiler_domain
module C = Sxfiler_renderer_core
module A = C.Application_context

module State = struct
  type message = C.Message.t
  type t = {app_context : A.t}

  let make () = {app_context = A.make File_tree}

  let reduce t = function
    | C.Message.(Command Planning) -> {app_context = A.push_mode t.app_context ~mode:Preview}
    | C.Message.(Command Approve) | Command Reject -> {app_context = A.pop_mode t.app_context}
    | C.Message.Finish_bootstrap -> {app_context = A.make File_tree}
    | C.Message.Initialize_omnibar -> {app_context = A.push_mode t.app_context ~mode:Complete}
    | C.Message.Finalize_omnibar -> {app_context = A.pop_mode t.app_context}
    | _ -> t

  (** [match_current_mode t ~mode] returns current mode of [t] is same or not [mode] *)
  let match_current_mode t ~mode = A.current_mode t.app_context = mode

  let current_mode t = A.current_mode t.app_context
  let equal : t -> t -> bool = ( = )
end

module Store = C.Store.Make (State)
