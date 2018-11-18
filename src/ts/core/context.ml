include Context_intf

let make_instance (type c) context config =
  let module C = (val context : S with type config = c) in
  ( module struct
    module Context = C

    let this = C.create config
  end
  : Instance )
