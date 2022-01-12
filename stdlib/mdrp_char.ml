include Stdlib.Char
module Set = Set.Make (Char)

let order c =
  match c with
  | 'a' .. 'z' -> Char.code c - 96
  | 'A' .. 'Z' -> Char.code c - 64
  | _ -> assert false

let of_digit i =
  match i with
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | _ -> failwith "not a digit"

let to_digit = function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | _ -> failwith "not a digit"

let check c =
  let v = Char.code c in
  v >= 32 && v <= 125 && v <> 96

let pp ppf c = Format.fprintf ppf "%c" c

let concat c1 c2 = Format.sprintf "%c%c" c1 c2

let to_string c = Format.sprintf "%c" c
