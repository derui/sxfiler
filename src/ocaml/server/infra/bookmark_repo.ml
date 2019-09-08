module C = Sxfiler_server_core
module D = Sxfiler_domain

(** Make scanner repository with backend. *)
module Make (S : C.Statable.S with type state = D.Bookmark.t list) : D.Bookmark_repository.S =
struct
  let resolve id =
    S.with_lock (fun state ->
        List.find_opt (fun v -> D.Bookmark.equal_id v.D.Bookmark.id id) state |> Lwt.return)

  let find_all () =
    let module C = Sxfiler_core in
    S.with_lock Lwt.return

  let find_by_path path =
    let module C = Sxfiler_core in
    S.with_lock (fun state ->
        List.find_opt (fun (v : D.Bookmark.t) -> C.Path.equal v.path path) state |> Lwt.return)

  let remove (t : D.Bookmark.t) =
    S.with_lock (fun state ->
        let state' = List.filter (fun v -> not @@ D.Bookmark.have_same_id v t) state in
        S.update state')

  let store (t : D.Bookmark.t) =
    S.with_lock (fun state ->
        let state' = t :: List.filter (fun v -> not @@ D.Bookmark.have_same_id v t) state in
        S.update state')
end
