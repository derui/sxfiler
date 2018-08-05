
include module type of struct include Usecase_intf end

(** [make_instance module config] create a new instance of Dispatcher. *)
val make_instance:
  (module S with type param = 'p)
  -> param:'p
  -> (module Instance)
