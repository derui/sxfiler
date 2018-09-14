module C = Sxfiler_renderer_core
module T = Sxfiler_rpc.Types
module S = Sxfiler_renderer_store

(** [plan] defines type to make plan with planner.  *)
type plan = C.Types.corrections -> action:(module Types.Action) -> unit Lwt.t

(** [execute] defines type to execute plan.  *)
type execute = action:(module Types.Action) -> unit Lwt.t

type executor =
  { plan : plan
  ; execute : execute }

type operation =
  | Approve
  | Reject
  | Conflict of T.Node.t list

type t =
  { mutable current_corrections : T.Node.t list
  ; executor_signal : executor Lwt_condition.t
  ; executor_mutex : Lwt_mutex.t
  ; action : (module Types.Action) }

let instance = ref None

(* the singleton instance for planner *)
let initialize store =
  instance :=
    Some
      { current_corrections = []
      ; executor_signal = Lwt_condition.create ()
      ; executor_mutex = Lwt_mutex.create ()
      ; action =
          ( module struct
            let state () = store
          end ) }

(* [start ()] runs planner main loop asynchronous. This function returns tuple [(sender, stopper)].
   User can send message via [sender] and stop loop via [stopper]. *)
let start () =
  let open Sxfiler_core.Option.Infix in
  !instance
  >|= fun instance ->
  let accepter : C.Message.t Lwt_mvar.t = Lwt_mvar.create_empty () in
  let message_accepter message = Lwt.async (fun () -> Lwt_mvar.put accepter message) in
  let rec main_loop t =
    let%lwt executor = Lwt_condition.wait t.executor_signal in
    let%lwt () =
      Lwt_mutex.with_lock t.executor_mutex (fun () ->
          let%lwt () = executor.plan t.current_corrections ~action:t.action in
          let rec loop () =
            match%lwt Lwt_mvar.take accepter with
            | C.Message.(Command Command.Approve) -> executor.execute ~action:t.action
            | C.Message.(Command Command.Reject) -> Lwt.return_unit
            | C.Message.(Command (Command.Conflict corrections)) ->
              let corrections = List.flatten [t.current_corrections; corrections] in
              t.current_corrections <- corrections ;
              let%lwt () = executor.plan corrections ~action:t.action in
              loop ()
            | _ -> loop ()
          in
          loop () )
    in
    main_loop t
  in
  let loop = main_loop instance in
  Lwt.async (fun () -> loop) ;
  (message_accepter, fun () -> Lwt.cancel loop)

(* [reserve_executor executor] reserves executor to next planning *)
let reserve_executor executor =
  let open Sxfiler_core in
  let open Option.Infix in
  let open Fun in
  !instance
  >>= fun instance -> (Lwt_condition.signal instance.executor_signal %> Option.some) executor
