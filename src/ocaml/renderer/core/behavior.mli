
include module type of (struct include Behavior_intf end)

(** [make_instance module config] create a new instance of Dispatcher. *)
val make_instance:
  (module S with type param = 'p and type config = 'a)
  -> config:'a
  -> param:'p
  -> (module Instance)
