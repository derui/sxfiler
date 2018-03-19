module T = Common_types

module type Completion_item = sig
  type t
  type js

  val to_js : t -> js Js.t
  val of_js : js Js.t -> t
end

module type Completion_state = sig
  type item
  type item_js

  type t = {
    cursor_pos : T.cursor_pos;
    items : item array;
    candidates : item array;
    completing : bool;
    prev_input : string;
  }

  class type js = object
    method candidates : item_js Js.t Js.js_array Js.t Js.readonly_prop
    method completing : bool Js.t Js.readonly_prop
    method cursorPos : int Js.readonly_prop
    method items : item_js Js.t Js.js_array Js.t Js.readonly_prop
    method prevInput : Js.js_string Js.t Js.readonly_prop
  end

  val empty : t
  val select_next : t -> t
  val select_prev : t -> t
  val refresh : t -> candidates:item array -> t
  val complete : t -> input:string -> items:item array -> t
  val start_completion : t -> t
  val finish_completion : t -> t
  val selected : t -> item
  val to_js : t -> js Js.t
  val of_js : js Js.t -> t
end

module type Completion_state_instance = sig
  module Completion_state: Completion_state

  val instance: Completion_state.t
end
