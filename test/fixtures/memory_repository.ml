open Sxfiler_core

module D = Sxfiler_domain
(** Define in-memory repositories *)

(** make new in-memory repository for Filer *)
let filer_repository ?(initial = []) () =
  let hash : (D.Filer.id, D.Filer.t) Hashtbl.t = Hashtbl.create 10 in
  List.iter (fun t -> Hashtbl.add hash t.D.Filer.id t) initial ;
  ( module struct
    let resolve id = Hashtbl.find hash id |> Lwt.return

    let resolve_by_name name =
      Hashtbl.to_seq hash |> Seq.map snd |> List.of_seq
      |> List.find_opt (fun v -> v.D.Filer.name = name)
      |> Lwt.return

    let store t = Hashtbl.add hash t.D.Filer.id t |> Lwt.return
  end : D.Filer.Repository )

(** make new in-memory repository for Filer *)
let key_map_repository init =
  let data = ref init in
  ( module struct
    let resolve () = Lwt.return !data

    let store t =
      data := t ;
      Lwt.return_unit
  end : D.Key_map_repository.S )

(** make new in-memory repository for Bookmark *)
let bookmark_repository init =
  let data = ref init in
  ( module struct
    let resolve id =
      List.find_opt (fun v -> D.Bookmark.equal_id v.D.Bookmark.id id) !data |> Lwt.return

    let find_all () = Lwt.return !data

    let find_by_path path =
      List.find_opt (fun v -> Path.equal v.D.Bookmark.path path) !data |> Lwt.return

    let store t =
      let data' = List.filter (fun v -> not @@ D.Bookmark.have_same_id v t) !data |> List.cons t in
      data := data' ;
      Lwt.return_unit
  end : D.Bookmark_repository.S )
