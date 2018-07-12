
val keyboard_event_to_key : Jsoo_reactjs.Event.Keyboard_event.t -> string

val find_item_index: ?equal:('a -> 'a -> bool) -> v:'a -> 'a array -> int

module Optdef : sig
  val (>>=) : 'a Js.optdef -> ('a -> 'b Js.optdef) -> 'b Js.optdef
end
