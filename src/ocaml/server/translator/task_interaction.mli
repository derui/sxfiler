(** Translators for {!Sxfiler_domain.Task_interaction}. This module can not support conversion from
    type defined in module to domain. *)

module Reply : sig
  include
    Core.Domain_translator
      with type t := Sxfiler_server_generated.Task.TaskReply.t
       and type domain := Sxfiler_domain.Task_interaction.Reply.t
end

module Suggestion : sig
  include
    Core.Domain_translator
      with type t := Sxfiler_server_generated.Task.TaskSuggestion.t
       and type domain := Sxfiler_domain.Task_interaction.Suggestion.t
end
