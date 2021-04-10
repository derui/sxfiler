open Abbrev

type event = Updated of D.Configuration_store.t [@@deriving eq, show]

module Update = struct
  type input = {
    key : D.Configuration_store.Key.t;
    value : Yojson.Basic.t;
  }

  type output = event list

  type 'a work_flow = input -> (output, 'a) S.t
end

type commands = Update of Update.input
