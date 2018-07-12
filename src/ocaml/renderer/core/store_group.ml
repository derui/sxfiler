(** This module provides to group stores into one store. Grouping stores is very difficult under
    static typing, Using tags as store resolves it in this module.
*)

(** The module provides grouping stores. *)
module type S = sig
  type t

  (** [create ()] returns new instance of {!t} *)
  val create: unit -> t

  (** [update t ~tag ~v] adds or updates value [v] as [tag]. *)
  val update: t -> tag:'a Tag.def -> v:'a -> t

  (** [subscribe t f] add [f] as subscription that is called when some tag is updated.  *)
  val subscribe: t -> (t -> unit) -> t

  (** [get t ~tag] gets a value specified [tag].

      @raise Fail raises if tag not found.
  *)
  val get: t -> tag:'a Tag.def -> 'a
end

module Core: S = struct
  type t = {
    subscriptions: (t -> unit) list;
    tag_table: Obj.t Jstable.t
  }

  let create () =
    let tag_table = Jstable.create () in
    {
      subscriptions = [];
      tag_table;
    }

  let get t ~tag =
    let key = Js.string @@ Tag.name tag in
    let v = Js.Optdef.to_option @@ Jstable.find t.tag_table key in
    match v with
    | None -> failwith "Unregistered tag"
    | Some v -> Obj.obj v

  let update t ~tag ~v =
    let key = Js.string @@ Tag.name tag in
    Jstable.add t.tag_table key (Obj.repr v);
    t

  let subscribe t f = {t with subscriptions = f :: t.subscriptions}
end

include Core
