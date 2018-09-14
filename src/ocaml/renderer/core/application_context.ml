(** Application_context provides management for flags of application.*)
type t =
  { base_focus : Types.Mode.t
  ; focus_stack : Types.Mode.t list }

(** [make base_mode] makes new instance of application context.  *)
let make base_focus = {base_focus; focus_stack = []}

(** [current_layer t] returns current layer of [t]. *)
let current_mode t = match t.focus_stack with [] -> t.base_focus | top :: _ -> top

(** [change_mode t ~mode] change current focusing mode to [mode].
*)
let push_mode t ~mode =
  if List.mem mode t.focus_stack then
    let focus_stack = List.filter (fun v -> v <> mode) t.focus_stack in
    {t with focus_stack = mode :: focus_stack}
  else {t with focus_stack = mode :: t.focus_stack}

(** [pop_mode t] pop the mode on top of the layer stack. Do not raise error if layers without basement is empty. *)
let pop_mode t = match t.focus_stack with [] -> t | _ :: rest -> {t with focus_stack = rest}
