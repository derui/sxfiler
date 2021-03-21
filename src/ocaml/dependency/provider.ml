open Sxfiler_core

module type S = sig
  type (+'a, 'r) t

  type void
  (** unified result. *)

  include Monad.S2 with type ('a, 'r) t := ('a, 'r) t

  module Context : sig
    type value

    type 'a t

    val value : 'a -> 'a t -> value
  end

  val run : ('a, void) t -> 'a
  (** [run program] run the [program] and get the result. *)

  val provide : ('r -> Context.value) -> ('a, 'r) t -> ('a, 'v) t
  (** [provide provider v] provides dependencies from context. *)

  val fetch : tag:('a Context.t -> 'r) -> ('a, 'r) t
  (** [fetch ~tag] get the monad to extract dependency from context. *)
end

module Impl : S = struct
  module Context = struct
    type value = exn

    type 'a t = 'a -> value

    let make f = f

    module type T = sig
      type t
    end

    module Make_box (T : T) = struct
      exception Box of T.t

      let unbox = function Box t -> t | _ -> failwith "Invalid unboxing"

      let box v = Box v
    end

    let value v t = t v

    let embed : unit -> 'a t * (value -> 'a) =
      fun (type s) () ->
       let module Box = Make_box (struct
         type t = s
       end) in
       let context = Box.box in
       let prj = Box.unbox in
       (context, prj)
  end

  type (+'a, 'r) t = 'r Context.t -> 'a

  type void

  include Monad.Make2 (struct
    type (+'a, 'r) t = 'r -> 'a

    let return : 'a -> ('a, 'r) t = fun v _ -> v

    let bind : ('a, 'r) t -> f:('a -> ('b, 'r) t) -> ('b, 'r) t = fun v ~f r -> f (v r) r

    let fmap = `Use_bind_to_define
  end)

  let run : ('a, void) t -> 'a = fun f -> f @@ Context.make (fun _ -> failwith "Invalid context")

  let provide provider t _ = t @@ Context.make provider

  let fetch ~tag =
    let box, unbox = Context.embed () in
    let key = tag @@ Context.make box in
    fun ctx -> unbox @@ Context.value key ctx
end

include Impl
