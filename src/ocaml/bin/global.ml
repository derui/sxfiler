module D = Sxfiler_domain
module C = Sxfiler_server_core
module T = Sxfiler_server_task

module Root = C.Statable.Make (struct
  type t = C.Root_state.t

  let empty () = C.Root_state.empty
end)

module Completer = struct
  type t = unit -> (module D.Completer.Instance)

  let instance : t ref = ref @@ fun () -> failwith "Not initialized"

  (** [set closure] set closure to wrap completer instance *)
  let set t = instance := t

  let get () = !instance ()
end

(* Cached source to complete in next operation. *)
module Cached_source = C.Statable.Make (struct
  type t = Sxfiler_domain.Completion.collection

  let empty () = []
end)

module Task_runner (G : D.Id_generator_intf.Gen_random with type id = Uuidm.t) : sig
  val get : unit -> (module T.Runner.Instance)
end = struct
  let t = ref None

  let get () =
    match !t with
    | None ->
        let v = T.Runner.make (module G) in
        t := Some v ;
        v
    | Some t -> t
end

module Keymap = C.Statable.Make (struct
  type t = D.Key_map.t

  let empty () = D.Key_map.make ()
end)

module Bookmark = C.Statable.Make (struct
  type t = D.Bookmark.t list

  let empty () = []
end)

module Configuration = C.Statable.Make (struct
  type t = Sxfiler_domain.Configuration.t

  let empty () = Sxfiler_domain.Configuration.default
end)

module Condition = C.Statable.Make (struct
  type t = Sxfiler_domain.Condition.t

  let empty () = Sxfiler_domain.Condition.empty
end)

(* Clock module to get current unix time *)
module Clock : D.Location_record.Clock = struct
  let unixtime () = Int64.of_float @@ Unix.gettimeofday ()
end
