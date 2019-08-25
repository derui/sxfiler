module C = Sxfiler_core
open Sxfiler_domain

module List_all = struct
  (** Module to share interface and structure. *)
  module Type = struct
    type input = unit
    type output = Bookmark.t list
    type error = unit
  end

  module type S =
    Common.Usecase
    with type input = Type.input
     and type output = Type.output
     and type error = Type.error

  (** This module defines usecase interface to get current key bindings. Replace [json] on
      implementation to match rpc. *)
  module Make (C : Bookmark_repository.S) : S = struct
    include Type

    let execute () =
      let open Lwt in
      C.find_all () >>= return_ok
  end
end

module Register = struct
  (** Module to share interface and structure. *)
  module Type = struct
    type input = {path : C.Path.t}
    type output = Bookmark.t
    type error = [`Conflict]
  end

  module type S =
    Common.Usecase
    with type input = Type.input
     and type output = Type.output
     and type error = Type.error

  module Make
      (Id : Id_generator_intf.Gen_random with type id = Bookmark.id)
      (C : Bookmark_repository.S) : S = struct
    include Type

    let execute {path} =
      match%lwt C.find_by_path path with
      | Some _ -> Lwt.return_error `Conflict
      | None ->
        let%lwt list = C.find_all () in
        let length = List.length list in
        let t = Bookmark.make ~id:Id.(generate ()) ~order:(succ length) ~path in
        let%lwt () = C.store t in
        Lwt.return_ok t
  end
end

module Delete = struct
  (** Module to share interface and structure. *)
  module Type = struct
    type input = {id : Bookmark.id}
    type output = Bookmark.t
    type error = [`Not_found]
  end

  module type S =
    Common.Usecase
    with type input = Type.input
     and type output = Type.output
     and type error = Type.error

  module Make (C : Bookmark_repository.S) : S = struct
    include Type

    let execute {id} =
      match%lwt C.resolve id with
      | None -> Lwt.return_error `Not_found
      | Some v ->
        let%lwt () = C.remove v in
        Lwt.return_ok v
  end
end
