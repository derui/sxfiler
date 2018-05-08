
val keyboard_event_to_key : Jsoo_reactjs.Event.Keyboard_event.t -> string

val get_focus_target: Sxfiler_common.State.Dialog_state.t -> Types.focus_target

val find_item_index: ?equal:('a -> 'a -> bool) -> v:'a -> 'a array -> int
