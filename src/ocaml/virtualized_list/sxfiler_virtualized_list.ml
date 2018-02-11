module R = Reactjscaml

type 'a t = {
  all_items: 'a array;
  visible_window: int * int;
  list_element: Dom_html.element Js.t option;
  item_cache: Dom_html.element Js.t array;
}

let empty = {
  all_items = [||];
  visible_window = (0, 0);
  list_element = None;
  item_cache = [||];
}

let update_all_items all_items t = {t with all_items}
let update_list_element el t = {t with list_element = Some el}
let update_item_cache els t = {t with item_cache = els}

let get_client_height : Dom_html.element Js.t option -> float = fun el ->
  match el with
  | None -> 0.0
  | Some el -> begin
      let rect = el##getBoundingClientRect in
      Js.Optdef.get rect##.height (fun () -> 0.0)
    end

let get_item_height items =
  if Array.length items = 0 then 0.0
  else get_client_height (Some items.(0))

let calc_viewable_item_count list_height item_height  =
  int_of_float @@ ceil @@ list_height /. item_height

let calc_visible_window t visible_count cursor_position =
  let prev_start, prev_count = t.visible_window in
  let prev_end = prev_start + pred prev_count in
  let all_item_count = Array.length t.all_items in

  if visible_count >= all_item_count then (0, all_item_count)
  else if cursor_position < prev_start then (cursor_position, visible_count)
  else if cursor_position > prev_end then begin
    let diff = abs (cursor_position - prev_start) in
    let start = prev_start + diff in

    if start + visible_count <= all_item_count then (prev_start + diff, visible_count)
    else (prev_start + diff - ((start + visible_count) - all_item_count), visible_count)
  end
  else (prev_start, visible_count)

let recalculate_visible_window cursor_position t =
  let list_height = get_client_height t.list_element in
  let item_height = get_item_height t.item_cache in
  let visible_count = calc_viewable_item_count list_height item_height in
  let visible_window = calc_visible_window t visible_count cursor_position in
  {t with visible_window = visible_window}

let get_items_in_window t = let window_start, window_size = t.visible_window in
  Array.sub t.all_items window_start window_size

let percentage_by_visible t =
  let _, size = t.visible_window in
  let size = float_of_int size
  and size_of_all = float_of_int @@ Array.length t.all_items in
  size /. size_of_all
