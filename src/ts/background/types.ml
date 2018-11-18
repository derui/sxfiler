module S = Sxfiler_renderer_store

module type Action = sig
  val state : unit -> S.App.State.t
end
