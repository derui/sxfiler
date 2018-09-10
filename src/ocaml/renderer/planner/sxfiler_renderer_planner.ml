module C = Sxfiler_renderer_core
module T = Sxfiler_rpc.Types
module S = Sxfiler_renderer_store

module type Action = sig
  val state : unit -> S.App.State.t
end

(** [plan] defines type to make plan with planner.  *)
type plan =
  C.Types.corrections
  -> dispatcher:(module C.Dispatcher.Instance)
  -> action:(module Action)
  -> unit Lwt.t

(** [execute] defines type to execute plan.  *)
type execute = dispatcher:(module C.Dispatcher.Instance) -> action:(module Action) -> unit Lwt.t

type executor =
  { plan : plan
  ; execute : execute }

type operation =
  | Approve
  | Reject
  | Conflict of T.Node.t list

type t =
  { mutable current_executor : executor option
  ; mutable current_corrections : T.Node.t list
  ; executor_signal : executor Lwt_condition.t
  ; executor_mutex : Lwt_mutex.t
  ; dispatcher : (module C.Dispatcher.Instance)
  ; action : (module Action) }

let instance = ref None

(* the singleton instance for planner *)
let initialize (module I : C.Dispatcher.Instance) store =
  instance :=
    Some
      { current_executor = None
      ; current_corrections = []
      ; executor_signal = Lwt_condition.create ()
      ; executor_mutex = Lwt_mutex.create ()
      ; dispatcher = (module I)
      ; action =
          ( module struct
            let state () = store
          end ) }

let setup t =
  let accepter : C.Message.t Lwt_condition.t = Lwt_condition.create () in
  let message_accepter message = Lwt_condition.signal accepter message in
  let rec main_loop t =
    let%lwt executor = Lwt_condition.wait t.executor_signal in
    let%lwt () =
      Lwt_mutex.with_lock t.executor_mutex (fun () ->
          let%lwt () =
            executor.plan t.current_corrections ~dispatcher:t.dispatcher ~action:t.action
          in
          let rec loop () =
            match%lwt Lwt_condition.wait accepter with
            | C.Message.(Command Command.Approve) ->
              executor.execute ~dispatcher:t.dispatcher ~action:t.action
            | C.Message.(Command Command.Reject) -> Lwt.return_unit
            | C.Message.(Command (Command.Conflict corrections)) ->
              let corrections = List.flatten [t.current_corrections; corrections] in
              t.current_corrections <- corrections ;
              let%lwt () = executor.plan corrections ~dispatcher:t.dispatcher ~action:t.action in
              loop ()
            | _ -> loop ()
          in
          loop () )
    in
    main_loop t
  in
  (message_accepter, fun () -> main_loop t)
