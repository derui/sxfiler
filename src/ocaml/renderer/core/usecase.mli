include module type of struct
  include Usecase_intf
end

val make_instance : (module S with type param = 'p) -> param:'p -> (module Instance)
(** [make_instance module config] create a new instance of Dispatcher. *)
