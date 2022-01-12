module F = Format

module Year = struct
  type t = int

  let is_leap year = (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0

  let of_int i = i
end

module Month = struct
  type t =
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

  let of_int = function
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
    | m -> invalid_arg (Format.sprintf "%d is not a valid month number" m)

  let nb_days ?(year : Year.t = 1901) = function
    | Jan | Mar | May | Jul | Aug | Oct | Dec -> 31
    | Feb -> if Year.is_leap year then 29 else 28
    | _ -> 30

  let to_int = function
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

  let pp ppf m =
    Format.fprintf ppf "%s"
      (match m with
      | Jan -> "January"
      | Feb -> "February"
      | Mar -> "March"
      | Apr -> "April"
      | May -> "May"
      | Jun -> "June"
      | Jul -> "July"
      | Aug -> "August"
      | Sep -> "September"
      | Oct -> "October"
      | Nov -> "November"
      | Dec -> "December")

  let list = [ Jan; Feb; Mar; Apr; May; Jun; Jul; Aug; Sep; Oct; Nov; Dec ]
end

module Day = struct
  type t = int

  let of_int d =
    if d < 0 || d > 32 then
      invalid_arg (Format.sprintf "%d is not a valid day" d)
    else d
end

module WeekDay = struct
  type t = Mon | Tue | Wed | Thu | Fri | Sat | Sun

  let of_int = function
    | 0 -> Mon
    | 1 -> Tue
    | 2 -> Wed
    | 3 -> Thu
    | 4 -> Fri
    | 5 -> Sat
    | 6 -> Sun
    | d ->
        invalid_arg
          (Format.sprintf
             "%d is not a valid week day. Should be between 0(Monday) and \
              6(Sunday)."
             d)

  let to_int = function
    | Mon -> 0
    | Tue -> 1
    | Wed -> 2
    | Thu -> 3
    | Fri -> 4
    | Sat -> 5
    | Sun -> 6
end

type t = { day : Day.t; month : Month.t; year : Year.t }

let create ~day ~month ~year =
  let day = Day.of_int day in
  let month = Month.of_int month in
  let year = Year.of_int year in
  let nbd = Month.nb_days ~year month in
  if day > nbd then
    invalid_arg
      (Format.asprintf "%a contains %d days. %d is greater than %d" Month.pp
         month nbd day nbd);
  { day; month; year }

module Format = struct
  type t = MDY | YMD | DMY
end

module Sep = struct
  type t = Slash | Period | Dash | Space

  let to_char = function
    | Slash -> '/'
    | Period -> '.'
    | Dash -> '-'
    | Space -> ' '
end

open Format
open Sep

let of_string ?(format = DMY) ?(sep = Slash) s =
  let l = String.split_on_char (Sep.to_char sep) s in
  match l with
  | [ one; two; three ] ->
      let day, month, year =
        match format with
        | DMY -> (one, two, three)
        | YMD -> (three, two, one)
        | MDY -> (two, one, three)
      in
      {
        day = int_of_string day;
        month = Month.of_int (int_of_string month);
        year = int_of_string year;
      }
  | _ -> failwith "Not a valid date string"

let pp_custom ?(format = DMY) ?(sep = Slash) () ppf { day; month; year } =
  let sep = Sep.to_char sep in
  let month = Month.to_int month in
  let one, two, three =
    match format with
    | DMY -> (day, month, year)
    | YMD -> (year, month, day)
    | MDY -> (month, day, year)
  in
  F.fprintf ppf "%d%c%d%c%d" one sep two sep three

let pp = pp_custom ~format:DMY ~sep:Slash ()

let equal d1 d2 = d1.day = d2.day && d1.month = d2.month && d1.year = d2.year

let ( = ) = equal
