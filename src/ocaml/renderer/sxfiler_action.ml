module F = Flux_frp.Std

module Core = struct
  type t = REQUEST_FILES_IN_DIRECTORY of string

  module State = Sxfiler_state

  let dispatch = function
    | REQUEST_FILES_IN_DIRECTORY s -> Lwt.return @@ Sxfiler_message.REQUEST_FILES_IN_DIRECTORY s
end

include F.Action_adapter.Make(Core)(Sxfiler_flux_runner)
