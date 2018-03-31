(**
   Providing stateful list virtualization.
*)

(** The virtualized list type to calculate something of virtualized list *)
type 'a t

(** Get an empty virtualized list *)
val make: ?item_height:int -> unit -> 'a t

(** Update totally items *)
val update_all_items : 'a array -> 'a t -> 'a t

(** Update list element that is DOM of list to manage with this type *)
val update_list_height : int -> 'a t -> 'a t

(** Update item element cache that is used to calculate  *)
val update_item_height : int -> 'a t -> 'a t

(** Recalculate visible window that is range of items to show *)
val recalculate_visible_window : int -> 'a t -> 'a t

(** get items in current visible window *)
val get_items_in_window : 'a t -> 'a array

(** Get the percentage by visible item in all items *)
val percentage_by_visible: 'a t -> float
