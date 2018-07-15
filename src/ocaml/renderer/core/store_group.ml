(** This module provides to group stores into one store. Grouping stores is very difficult under
    static typing, Using tags as store resolves it in this module.
*)

(** The module provides grouping stores. *)
module type S = sig
  type message
  type t
  type event = [`Dispatch of message -> unit | `Change of t -> unit]

  (** [create ()] returns new instance of {!t} *)
  val create: unit -> t

  (** [set t ~tag ~v] adds or updates value [v] as [tag]. *)
  val set: t -> tag:('a, _) Tag.def -> v:'a -> t

  (** [subscribe t event] add [event] as subscription that is called when some tag is updated.  *)
  val subscribe: t -> event:event -> t

  (** [get t ~tag] gets a value specified [tag].

      @raise Fail raises if tag not found.
  *)
  val get: t -> tag:('a, _) Tag.def -> 'a

  (** [dispatch t ~tag ~message] dispatch message to store specified [tag].  *)
  val dispatch: t -> message:message -> unit
end

module Core: S with type message := Message.t = struct

  type t = {
    change_subscribers: (t -> unit) list;
    dispatch_subscribers: (Message.t -> unit) list;
    tag_table: Obj.t Jstable.t
  }
  type event = [`Dispatch of Message.t -> unit | `Change of t -> unit]

  let create () =
    let tag_table = Jstable.create () in
    {
      dispatch_subscribers = [];
      change_subscribers = [];
      tag_table;
    }

  let get t ~tag =
    let key = Js.string @@ Tag.name tag in
    let v = Js.Optdef.to_option @@ Jstable.find t.tag_table key in
    match v with
    | None -> failwith "Unregistered tag"
    | Some v -> Obj.obj v

  let set t ~tag ~v =
    let key = Js.string @@ Tag.name tag in
    Jstable.add t.tag_table key (Obj.repr v);
    t

  let dispatch t ~message =
    List.iter (fun f -> f message) t.dispatch_subscribers

  let subscribe t ~(event:event) =
    match event with
    | `Dispatch f -> {t with dispatch_subscribers = f :: t.dispatch_subscribers}
    | `Change f -> {t with change_subscribers = f :: t.change_subscribers}
end

include Core
