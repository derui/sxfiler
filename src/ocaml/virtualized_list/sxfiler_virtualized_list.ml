module R = Jsoo_reactjs

type 'a t = {
  all_items: 'a array;
  visible_window: int * int;
  list_height: int option;
  item_height: int option;
}

let make ?item_height () = {
  all_items = [||];
  visible_window = (0, 0);
  list_height = None;
  item_height = item_height;
}

let update_all_items all_items t = {t with all_items}
let update_list_height h t = {t with list_height = Some h}
let update_item_height h t = {t with item_height = Some h}

let calc_viewable_item_count list_height item_height  =
  let option_to_int = function
    | None -> 1
    | Some v -> v
  in
  let list_height = float_of_int @@ option_to_int list_height
  and item_height = float_of_int @@ option_to_int item_height in
  int_of_float @@ ceil @@ list_height /. item_height

let calc_visible_window t visible_count cursor_position =
  let prev_start, prev_count = t.visible_window in
  let prev_end = prev_start + (max 0 @@ pred prev_count) in
  let all_item_count = Array.length t.all_items in

  if visible_count >= all_item_count then (0, all_item_count)
  else if cursor_position < prev_start then (cursor_position, visible_count)
  else if cursor_position > prev_end then begin
    let diff = abs (cursor_position - prev_end) in
    let start = prev_start + diff in
    (start, visible_count)
  end
  else (prev_start, visible_count)

let correct_visible_window all_item_count (start, count) =
  if start + count <= all_item_count then (start, count)
  else (start - ((start + count) - all_item_count), count)

let recalculate_visible_window cursor_position t =
  let visible_count = calc_viewable_item_count t.list_height t.item_height in
  let visible_window = calc_visible_window t visible_count cursor_position in
  let visible_window = correct_visible_window (Array.length t.all_items) visible_window in
  {t with visible_window = visible_window}

let get_items_in_window t =
  let window_start, window_size = t.visible_window in
  Array.sub t.all_items window_start window_size

let start_position_of_window {visible_window;_} = fst visible_window

let percentage_by_visible t =
  let _, size = t.visible_window in
  let size = float_of_int size
  and size_of_all = float_of_int @@ Array.length t.all_items in
  size /. size_of_all
