module Common_step = struct
  module Filer = Common_step_filer
  module File_list = Common_step_file_list
  module Interaction = Common_step_interaction
  module Completer = Common_step_completer
  module Keymap = Common_step_keymap
  module Location_history = Common_step_location_history
  module Configuration = Common_step_configuration
  include Common_step_common
end

module Filer = Filer
module Keymap = Keymap
module Configuration = Configuration
module Completer = Completer

(** All event that will raise from this module *)
type event =
  | Filer     of Filer.event
  | Keymap    of Keymap.event
  | Completer of Completer.event
[@@deriving show, eq]
