open Sxfiler_core
module C = Sxfiler_renderer_core
module T = Sxfiler_rpc.Types

module Filer = struct
  type node = T.Node.t * bool

  (* type of filer. Using array for nodes is for speed up to toggle mark.
     When filer has large number of nodes, slow down per toggling action if use list.
  *)
  type t =
    { id : string
    ; location : string
    ; nodes : node array
    ; selected_item_index : int }

  let make (filer : T.Filer.t) =
    { id = filer.id
    ; location = filer.location
    ; selected_item_index = 0
    ; nodes = Array.of_list filer.nodes |> Array.map (fun node -> (node, false)) }


  let move_index t ~direction =
    let max_index = Array.length t.nodes in
    match direction with
    | `Next ->
      {t with selected_item_index = min (pred max_index) (succ t.selected_item_index)}
    | `Prev ->
      {t with selected_item_index = max 0 (pred t.selected_item_index)}


  (* toggle mark of current selected index *)
  let toggle_mark t =
    let index = t.selected_item_index in
    let copied = Array.copy t.nodes in
    (* do not touch current array for immutability. *)
    let node, marked = copied.(index) in
    copied.(index) <- (node, not marked) ;
    {t with nodes = copied}
end

module State = struct
  type message = C.Message.t

  type t =
    { left : Filer.t option
    ; right : Filer.t option
    ; current : C.Types.File_list_pos.t }

  let make () = {left = None; right = None; current = `Left}
  let left {left; _} = left
  let right {right; _} = right

  let update_filer t ~side ~f =
    match side with `Left -> {t with left = f t.left} | `Right -> {t with right = f t.right}


  let reduce t = function
    | C.Message.Update_filer (side, filer) ->
      update_filer t ~side ~f:(fun _ -> Option.some @@ Filer.make filer)
    | C.Message.Move_cursor_to_next ->
      update_filer t ~side:t.current ~f:(fun list ->
          Option.fmap ~f:Filer.(move_index ~direction:`Next) list )
    | C.Message.Move_cursor_to_prev ->
      update_filer t ~side:t.current ~f:(fun list ->
          Option.fmap ~f:Filer.(move_index ~direction:`Prev) list )
    | C.Message.Swap_filer -> (
        match t.current with `Left -> {t with current = `Right} | `Right -> {t with current = `Left}
      )
    | C.Message.Toggle_mark ->
      update_filer t ~side:t.current ~f:(Option.fmap ~f:Filer.toggle_mark)
    | _ ->
      t


  let equal _ _ = false
end

module Store = C.Store.Make (State)
