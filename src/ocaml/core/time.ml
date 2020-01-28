type date = {
  year : int;
  month : int;
  date : int;
}

type t = date * int64

(* avoid warning in unused operator *)
module Int64_op = struct
  let ( > ) v1 v2 = Int64.compare v1 v2 > 0
  let ( < ) v1 v2 = Int64.compare v1 v2 < 0
  let ( = ) v1 v2 = Int64.equal v1 v2
  let ( >= ) v1 v2 = not (v1 < v2)
  let ( <= ) v1 v2 = not (v1 > v2)
  let ( <> ) v1 v2 = not (v1 = v2)
  let ( - ) = Int64.sub
  let ( + ) = Int64.add
  let ( / ) = Int64.div
  let ( * ) = Int64.mul
  let ( % ) = Int64.rem
end
[@warning "-32"]

(** [month] is representation for month in a year *)
type month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec

(** [days_in_month is_leapyear month] returns days in the [month]. This function handles leap-year *)
let days_in_month is_leapyear = function
  | Jan -> 31
  | Feb -> if is_leapyear then 29 else 28
  | Mar -> 31
  | Apr -> 30
  | May -> 31
  | Jun -> 30
  | Jul -> 31
  | Aug -> 31
  | Sep -> 30
  | Oct -> 31
  | Nov -> 30
  | Dec -> 31

(** [month_to_int month] get 1-origin month of a year from [month] *)
let month_to_int = function
  | Jan -> 1
  | Feb -> 2
  | Mar -> 3
  | Apr -> 4
  | May -> 5
  | Jun -> 6
  | Jul -> 7
  | Aug -> 8
  | Sep -> 9
  | Oct -> 10
  | Nov -> 11
  | Dec -> 12

(** [month_of_int month] get [month] type from 1-origin month of a year *)
let month_of_int = function
  | 1 -> Jan
  | 2 -> Feb
  | 3 -> Mar
  | 4 -> Apr
  | 5 -> May
  | 6 -> Jun
  | 7 -> Jul
  | 8 -> Aug
  | 9 -> Sep
  | 10 -> Oct
  | 11 -> Nov
  | 12 -> Dec
  | _ as v -> failwith @@ Printf.sprintf "Unknown month: %d" v

(* helper constants *)
let sec = 1_000_000L
let sec_of_minute = Int64.mul 60L sec
let sec_of_hour = Int64.mul 60L sec_of_minute
let sec_of_day = Int64.mul 24L sec_of_hour

(** [is_leapyear] is true, if and only if a year is a leap year *)
let is_leapyear year =
  year mod 4 = 0 && year mod 400 <> 100 && year mod 400 <> 200 && year mod 400 <> 300

let days_since_start_of_year is_leapyear mm dd =
  let monthes =
    List.map month_to_int [ Jan; Feb; Mar; Apr; May; Jun; Jul; Aug; Sep; Oct; Nov; Dec ]
  in
  List.fold_left
    (fun accum month ->
      if month < mm then accum + (month_of_int month |> days_in_month is_leapyear) else accum)
    dd monthes

let get_month_and_date is_leapyear days =
  let rec get_month days monthes =
    match monthes with
    | [] -> (Dec, 31)
    | m :: monthes ->
        let days_for_m = days_in_month is_leapyear m in
        let days' = days - days_for_m in
        if days' <= 0 then (m, days) else get_month days' monthes
  in
  get_month days [ Jan; Feb; Mar; Apr; May; Jun; Jul; Aug; Sep; Oct; Nov; Dec ]

let microseconds_since_0 yy mm dd =
  let open Int64_op in
  let yy' = Int64.of_int yy in
  let leap_years_per_400 = yy' / 400L * 97L in
  let leap_years =
    let rec count_leap_year current count =
      if current > yy' % 400L then count
      else
        let count = if is_leapyear (Int64.to_int current) then Int64.succ count else count in
        count_leap_year (Int64.succ current) count
    in
    count_leap_year 0L leap_years_per_400
  in
  let not_leap_years = Int64.of_int yy - leap_years in
  let days = (leap_years * 366L) + (not_leap_years * 365L) in
  let msecs_in_years = days * sec_of_day in
  let msecs_since_start = days_since_start_of_year (is_leapyear yy) mm dd |> Int64.of_int in
  msecs_in_years + (msecs_since_start * sec_of_day)

(* microseconds from 0000-01-01 to 1970-01-01. *)
let microseconds_between_0_to_1970 = microseconds_since_0 1970 1 1

let to_int64 ({ year; month; date }, ns) =
  let open Int64_op in
  microseconds_since_0 year month date + ns - microseconds_between_0_to_1970

let of_int64 microseconds =
  let open Int64_op in
  let days = (microseconds + microseconds_between_0_to_1970) / sec_of_day in

  let rec calculate_year rest_days year =
    let open Int64_op in
    let days_in_year = if is_leapyear year then 366L else 365L in
    if rest_days < days_in_year then (year, rest_days)
    else
      let rest_days = rest_days - days_in_year and year = succ year in
      calculate_year rest_days year
  in
  let year, rest_days = calculate_year days 0 in
  let leap_year = is_leapyear year in
  let month, date = get_month_and_date leap_year (Int64.to_int rest_days) in
  let secs_in_day = (microseconds + microseconds_between_0_to_1970) % sec_of_day in
  ({ year; month = month_to_int month; date }, secs_in_day)

let min_ns = 0L
let max_ns = Int64.sub sec_of_day 1L
let min = ({ year = 0; month = 1; date = 1 }, min_ns)
let max = ({ year = 9999; month = 12; date = 31 }, max_ns)

let compare_date d1 d2 =
  let compare_year = Int.compare d1.year d2.year
  and compare_month = Int.compare d1.month d2.month
  and compare_date = Int.compare d1.date d2.date in
  if compare_year = 0 then if compare_month = 0 then compare_date else compare_month
  else compare_year

let compare (d1, ns1) (d2, ns2) =
  let compare_d = compare_date d1 d2 in
  if Int.equal 0 compare_d then Int64.compare ns1 ns2 else compare_d

let equal v1 v2 = Int.equal (compare v1 v2) 0

let of_float ns =
  let gradural, fraction = Float.modf ns in
  let fraction = Int64.of_float fraction
  and gradural = gradural *. Int64.to_float sec |> Int64.of_float in
  let ns = Int64_op.((fraction * sec) + gradural) in
  let t = of_int64 ns in
  if compare t min < 0 then None else if compare t max > 0 then None else Some t

let to_float t =
  let open Int64_op in
  let msec = to_int64 t in
  let secs = Int64.to_float (msec / sec)
  and msec = Int64.to_float (msec % sec) /. Int64.to_float sec in
  secs +. msec

let to_rfc3399 t =
  let { year; month; date }, ns = t in
  let open Int64_op in
  let hours = ns / sec_of_hour in
  let minutes = Int64.rem ns sec_of_hour / sec_of_minute in
  let seconds = Int64.rem ns sec_of_minute / sec in
  let ns = Int64.rem ns sec in
  Printf.sprintf "%04d-%02d-%02dT%02Ld:%02Ld:%02Ld.%06Ld-00:00" year month date hours minutes
    seconds ns

let pp fmt t =
  let rfc3399 = to_rfc3399 t in
  Format.fprintf fmt "%s" rfc3399
