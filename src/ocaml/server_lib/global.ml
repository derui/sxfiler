module C = Sxfiler_server_core
module T = Sxfiler_server_task

module Root = C.Statable.Make(struct
    type t = C.Root_state.t

    let empty () = C.Root_state.empty
  end)

module Completion = C.Statable.Make(struct
    type t = (module Sxfiler_server_completion.Completer.Instance)

    (* initial value is truly dummy implementation. *)
    let empty () = (module struct
                     module Completer = struct
                       type t = string
                       let read _ ~input:_ ~collection:_ ~stringify:_ = []

                     end
                     let instance = "not initialized"
                   end : Sxfiler_server_completion.Completer.Instance)
  end)

(* File source to complete in next operation. *)
module File_source = C.Statable.Make(struct
    type t = Sxfiler_types.Node.t list

    let empty () = []
  end)

(* Common source to complete in next operation. *)
module Common_source = C.Statable.Make(struct
    type t = Sxfiler_types.Completion.Common_item.t list
    let empty () = []
  end)

module Task_runner : sig
  val get: unit -> (module T.Runner.Instance)
end = struct
  let t = ref None

  let get () =
    match !t with
    | None -> let v = T.Runner.make () in
      t := Some v;
      v
    | Some t -> t
end

module Keybindings = C.Statable.Make(struct
    type t = Yojson.Safe.json

    let empty () = `Null
  end)

module Configuration = C.Statable.Make(struct
    type t = Sxfiler_types.Configuration.t

    let empty () = Sxfiler_types.Configuration.default
  end)
