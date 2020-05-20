module D = Sxfiler_domain
module I = Sxfiler_infrastructure

module Filer = I.Statable.Make (struct
  type t = D.Filer.t option

  let empty () = None
end)

module Completer = struct
  type t = unit -> (module D.Completer.Instance)

  let instance : t ref = ref @@ fun () -> failwith "Not initialized"

  (** [set closure] set closure to wrap completer instance *)
  let set t = instance := t

  let get () = !instance ()
end

(* Cached source to complete in next operation. *)
module Cached_collection = I.Statable.Make (struct
  type t = D.Completer.collection

  let empty () = []
end)

module Keymap = I.Statable.Make (struct
  type t = D.Keymap.t

  let empty () = D.Keymap.empty
end)

module Bookmarks = I.Statable.Make (struct
  type t = D.Bookmarks.t

  let empty () = D.Bookmarks.empty
end)

module Configuration_store = I.Statable.Make (struct
  type t = D.Configuration_store.t

  let empty () = D.Configuration_store.empty
end)

module Context = I.Statable.Make (struct
  type t = D.Context.t

  let empty () = D.Context.empty
end)
