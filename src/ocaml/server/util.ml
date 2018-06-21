(** this module provides utilities for module creation.  *)

(** separator between module and method *)
let method_separator = "/"

(** Make a name of method in module. *)
let make_method ~module_prefixes ~name =
  String.(concat method_separator module_prefixes) ^ method_separator ^ name
