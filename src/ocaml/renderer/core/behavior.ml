
include Behavior_intf

let make_instance
    (type p)
    (type c)
    dispatcher
    ~config
    ~param =
  let module B = (val dispatcher : S with type param = p and type config = c) in
  (module struct
    module Behavior = B
    let this = B.create config param
  end : Instance)
