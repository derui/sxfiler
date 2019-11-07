module C = Sxfiler_server_core
module D = Sxfiler_domain

(** Make completion repository with backend. *)
module Make (S : C.Statable.S with type state = D.Completion.collection) : D.Completion.Repository =
struct
  let store collection = S.with_lock (fun _ -> S.update collection)
  let resolve () = S.get ()
end
