module D = Sxfiler_domain

type event = Completed of D.Completer.candidates [@@deriving eq, show]

(** move location of file list placed on left side *)
module Complete = struct
  type input = { input : string }

  type work_flow = input -> event list Lwt.t
end

(** the workflow to initialize completer with collection *)
module Initialize = struct
  type input = { collection : D.Completer.collection }

  type work_flow = input -> unit Lwt.t
end

type commands =
  | Initialize of Initialize.input
  | Complete   of Complete.input
