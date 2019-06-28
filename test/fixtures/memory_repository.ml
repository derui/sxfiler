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
