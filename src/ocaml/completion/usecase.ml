include Usecase_intf

module Setup(R:Repository.Collection) = struct
  type input = {
    source: Domain.collection;
  }

  type output = unit Lwt.t

  let execute (input:input) : output =
    R.store input.source

end

module Read
    (Repo:Repository.Collection)
    (I:Completer_intf.Instance) = struct
  type input = {
    input: string;
  }

  type output = Domain.result Lwt.t

  let execute (input:input) : output =
    let%lwt collection = Repo.resolve () in
    let candidates = I.(Completer.read this
                          ~input:input.input
                          ~collection
                          ~stringify:(module struct
                                       type t = Domain.Item.t
                                       let to_string t = t.Domain.Item.value
                                     end)) in
    Lwt.return candidates

end
