(** [to_option result] one-way convert to option. *)
let to_option = function Ok v -> Some v | Error _ -> None

(** [get r] returns a value when [r] is {!Ok}, otherwise raise exception or else. *)
let get = function Ok v -> v | Error _ -> failwith "Can not get value from result"

include Monad.Make2 (struct
    type ('a, 'b) t = ('a, 'b) result

    let bind m ~f = match m with Error e -> Error e | Ok v -> f v
    let fmap = `Use_bind_to_define
    let return v = Ok v
  end)

include Infix
