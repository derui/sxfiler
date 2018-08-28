include module type of struct
  include Context_intf
end

val make_instance : (module S with type config = 'a) -> 'a -> (module Instance)
(** [make_instance module config] create a new instance of Dispatcher. *)
