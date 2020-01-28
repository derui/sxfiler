module Candidates : sig
  include
    Core.Domain_translator
      with type t := Sxfiler_generated.Completer.Completer.Candidate.t list
       and type domain := Sxfiler_domain.Completer.candidates
end

module Collection : sig
  include
    Core.Domain_translator
      with type t := Sxfiler_generated.Completer.Completer.Item.t list
       and type domain := Sxfiler_domain.Completer.collection
end
