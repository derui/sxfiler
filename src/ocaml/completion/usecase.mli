include module type of (struct include Usecase_intf end)

module Setup(R:Repository.Collection) : Setup
module Read
    (Repo:Repository.Collection)
    (I:Completer_intf.Instance)
  : Read
