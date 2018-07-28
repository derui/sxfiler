
include module type of (struct include Dispatcher_intf end)

(** [make_instance module config] create a new instance of Dispatcher. *)
val make_instance:
  (module S with type config = 'a)
  -> 'a
  -> (module Instance)
