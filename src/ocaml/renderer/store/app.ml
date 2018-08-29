module T = Sxfiler_domain
module C = Sxfiler_renderer_core

module State = struct
  type message = C.Message.t

  type t =
    { config : Config.Store.t
    ; file_list : File_list.Store.t
    ; keymap : Keymap.Store.t
    ; completion : Completion.Store.t
    ; command : Command.Store.t
    ; workspace : Workspace.Store.t }

  let make ~config ~file_list ~keymap ~completion ~command ~workspace =
    {config; file_list; keymap; completion; command; workspace}

  let reduce t message =
    Config.Store.dispatch t.config message ;
    File_list.Store.dispatch t.file_list message ;
    Keymap.Store.dispatch t.keymap message ;
    Completion.Store.dispatch t.completion message ;
    Command.Store.dispatch t.command message ;
    Workspace.Store.dispatch t.workspace message ;
    t

  let config {config; _} = config
  let file_list {file_list; _} = file_list
  let keymap {keymap; _} = keymap
  let completion {completion; _} = completion
  let command {command; _} = command
  let workspace {workspace; _} = workspace

  let equal v1 v2 =
    Config.(State.equal (Store.get v1.config) (Store.get v2.config))
    && File_list.(State.equal (Store.get v1.file_list) (Store.get v2.file_list))
    && Keymap.(State.equal (Store.get v1.keymap) (Store.get v2.keymap))
    && Completion.(State.equal (Store.get v1.completion) (Store.get v2.completion))
    && Command.(State.equal (Store.get v1.command) (Store.get v2.command))
    && Workspace.(State.equal (Store.get v1.workspace) (Store.get v2.workspace))
end

module Store =
  C.Store.Make_group
    (State)
    (struct
      type state = State.t

      let watch_state callback state =
        let f _ = callback state in
        Config.Store.subscribe state.State.config ~f ;
        File_list.Store.subscribe state.State.file_list ~f ;
        Keymap.Store.subscribe state.State.keymap ~f ;
        Completion.Store.subscribe state.State.completion ~f ;
        Command.Store.subscribe state.State.command ~f ;
        Workspace.Store.subscribe state.State.workspace ~f
    end)
