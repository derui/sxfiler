type sign =
  | Plus
  | Minus

type offset =
  | Z
  | Num_offset of sign * int * int

type t =
  | Full_year of string
  | Month     of int
  | Mday      of int
  | Hour      of int
  | Minute    of int
  | Second    of int
  | Sec_frac  of int
  | Offset    of offset

type full_year = {
  year : int;
  month : int;
  mday : int;
}

type full_time = {
  hour : int;
  minute : int;
  second : int;
  sec_frac : int option;
  offset : offset;
}

type date_time = {
  full_year : full_year;
  full_time : full_time;
}

module Validator = struct
  let year_range { full_year = { year; _ }; _ } = year >= 0 && year <= 9999

  let month_range { full_year = { month; _ }; _ } = month >= 1 && month <= 12

  let date_range is_leapyear { full_year = { year; month; mday; _ }; _ } =
    let max_date =
      match month with
      | 1  -> 31
      | 2  -> if is_leapyear year then 29 else 28
      | 3  -> 31
      | 4  -> 30
      | 5  -> 31
      | 6  -> 30
      | 7  -> 31
      | 8  -> 31
      | 9  -> 30
      | 10 -> 31
      | 11 -> 30
      | 12 -> 31
      | _  -> -1
    in
    mday >= 1 && mday <= max_date

  let hour_range { full_time = { hour; _ }; _ } = hour >= 0 && hour <= 23

  let minute_range { full_time = { minute; _ }; _ } = minute >= 0 && minute <= 59

  let second_range { full_time = { second; _ }; _ } = second >= 0 && second <= 59

  let offset_range { full_time = { offset; _ }; _ } =
    match offset with
    | Z                            -> true
    | Num_offset (_, hour, minute) -> hour >= 0 && hour <= 23 && minute >= 0 && minute <= 59

  let frac_range { full_time = { sec_frac; _ }; _ } =
    match sec_frac with None -> true | Some v -> v >= 0 && v <= 999_999
end

let validate validators date_time = if List.for_all (fun v -> v date_time) validators then Some date_time else None
