
include Dispatcher_intf

let make_instance
    (type c)
    dispatcher
    config =
  let module D = (val dispatcher : S with type config = c) in
  (module struct
    module Dispatcher = D
    let this = D.create config
  end : Instance)
