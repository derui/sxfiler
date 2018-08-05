
include Usecase_intf

let make_instance
    (type p)
    usecase
    ~param =
  let module B = (val usecase : S with type param = p) in
  (module struct
    module Usecase = B
    let this = B.create param
  end : Instance)
