include Core
module Completion = S_completion
module File_list = S_file_list
module Global = S_global

let expose_static registry hub =
  let registry = S_completion.expose registry hub in
  let registry = S_file_list.expose registry hub in
  S_global.expose registry hub

let[@warning "-27"] expose_dynamic registry hub = registry
