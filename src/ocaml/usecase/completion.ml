module D = Sxfiler_domain

module Setup = struct
  module Type = struct
    type input = {source : D.Completion.collection}
    type output = unit
    type error = unit
  end

  module type S = sig
    include module type of Type

    include
      Common.Usecase
        with type input := Type.input
         and type output := Type.output
         and type error := Type.error
  end

  module Make (R : D.Completion.Repository) : S = struct
    include Type

    let execute (input : input) =
      let open Lwt in
      R.store input.source >>= Lwt.return_ok
  end
end

module Read = struct
  module Type = struct
    type input = {input : string}
    type output = D.Completion.candidates
    type error = unit
  end

  module type S =
    Common.Usecase
      with type input = Type.input
       and type output = Type.output
       and type error = Type.error

  module Make (Repo : D.Completion.Repository) (I : D.Completer.Instance) : S = struct
    include Type

    let execute (input : input) =
      let%lwt collection = Repo.resolve () in
      let candidates = I.(Completer.read this ~input:input.input ~collection) in
      Lwt.return_ok candidates
  end
end
