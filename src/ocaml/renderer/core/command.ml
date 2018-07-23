(** {!Command} defines types for command pallet. *)
module Class = struct
  type t =
    | Scanner_jump
    | Empty
end

module Param = struct
  type t = {
    name: string;
    value: < > Js.t
  }
end

type t = {
  current_class: Class.t;
  params: Param.t list;
}
