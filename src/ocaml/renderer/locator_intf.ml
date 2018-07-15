
open Sxfiler_renderer_core

module type S = sig
  module Repository : sig
    module Scanner : Repository_intf.Scanner_instance
  end
end
