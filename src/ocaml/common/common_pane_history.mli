
module T = Common_types
module History :
sig
  type t = {
    directory : string;
    focused_item : T.File_id.t option;
    timestamp : int64;
  }

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t

  class type js = object
    method directory : Js.js_string Js.t Js.readonly_prop
    method focusedItem : T.File_id.js Js.t Js.opt Js.readonly_prop
    method timestamp : float Js.readonly_prop
  end

  val make :
    ?timestamp:int64 ->
    directory:string -> focused_item:T.File_id.t option -> unit -> t

  val to_js : t -> js Js.t
  val of_js : js Js.t -> t
end

type t

class type js = object
  method historyMap : History.js Js.t Jstable.t Js.readonly_prop
  method maxStoreableCount : Js.number Js.t Js.readonly_prop
end

val make : unit -> t
val to_js : t -> js Js.t
val of_js : js Js.t -> t

val add_history : history:History.t -> t -> t

val restore_pane_info : pane:Common_types.Pane.t ->
  t -> Common_types.Pane.t

val sorted_history : t -> History.t array

module Test : sig
  val suite : unit -> unit
end