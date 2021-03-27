module S = Sxfiler_dependency
module D = Sxfiler_domain
module C = Common_step_completer

type event = Completed of D.Completer.candidates [@@deriving eq, show]

(** move location of file list placed on left side *)
module Complete = struct
  type input = { input : string }

  type work_flow =
    input ->
    ( event list Lwt.t,
      [ `Step_completer_provide_collection of C.provide_collection S.Context.t
      | `Completer_instance                of (module D.Completer.Instance) S.Context.t
      ] )
    S.t
end

(** the workflow to initialize completer with collection *)
module Initialize = struct
  type input = { collection : D.Completer.collection }

  type work_flow =
    input ->
    (unit Lwt.t, [ `Step_completer_update_collection of Common_step_completer.update_collection S.Context.t ]) S.t
end

type commands =
  | Initialize of Initialize.input
  | Complete   of Complete.input
