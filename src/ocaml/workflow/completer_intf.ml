module S = Sxfiler_dependency
module D = Sxfiler_domain
module C = Common_step_completer

type event = Completed of D.Completer.candidates [@@deriving eq, show]

(** move location of file list placed on left side *)
module Complete = struct
  type input = { input : string }
end

(** the workflow to initialize completer with collection *)
module Initialize = struct
  type input = { collection : D.Completer.collection }
end

type commands =
  | Initialize of Initialize.input
  | Complete   of Complete.input
