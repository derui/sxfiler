module T = Sxfiler_types

type t = {
  viewer_stacks: Types.Viewer_stack.t Jstable.t;
  config: T.Configuration.t;
  workspace_order: string list;
}

let empty () = {
  viewer_stacks = Jstable.create ();
  config = T.Configuration.default;
  workspace_order = [Const.workspace_1;Const.workspace_2]
}

let with_stack t ~name ~f =
  let stack = Jstable.find t.viewer_stacks Js.(string name) |> Js.Optdef.to_option in
  let stack = f stack in
  Jstable.add t.viewer_stacks Js.(string name) stack;
  t
