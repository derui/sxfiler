module Candidates : sig
  include
    Core.Domain_translator
      with type t := Sxfiler_server_generated.Completion.Candidate.t list
       and type domain := Sxfiler_domain.Completion.candidates
end

module Collection : sig
  include
    Core.Domain_translator
      with type t := Sxfiler_server_generated.Completion.Item.t list
       and type domain := Sxfiler_domain.Completion.collection
end
