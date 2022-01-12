exception NotInBound of int * (int * int)

module Value = struct
  type t = King | Queen | Jake | Number of int

  (* param is in [0..12] because it comes from i mod 13  *)
  let of_int = function
    | 0 -> King
    | 1 -> Queen
    | 2 -> Jake
    | n when 2 < n && n < 13 -> Number (13 - n)
    | n -> raise (NotInBound (n, (0, 12)))

  let pp fmt v =
    Format.fprintf fmt "%s"
      (match v with
      | King -> "K"
      | Queen -> "Q"
      | Jake -> "J"
      | Number n -> Format.sprintf "%d" n)

  let equal v1 v2 =
    match (v1, v2) with
    | King, King | Queen, Queen | Jake, Jake -> true
    | Number n1, Number n2 -> n1 = n2
    | _ -> false

  let compare v1 v2 =
    if equal v1 v2 then 0
    else
      match (v1, v2) with
      | Number 1, _ -> 1
      | _, Number 1 -> -1
      | King, _ -> 1
      | _, King -> -1
      | Queen, _ -> 1
      | _, Queen -> -1
      | Jake, _ -> 1
      | _, Jake -> -1
      | Number n1, Number n2 -> (n1 - n2) / abs (n1 - n2)

  let follows v1 v2 =
    match (v1, v2) with
    | Number n1, Number n2 -> n1 = n2 + 1
    | Queen, Jake | King, Queen | Number 1, King | Jake, Number 10 -> true
    | _ -> false

  let of_char = function
    | 'K' -> King
    | 'Q' -> Queen
    | 'J' -> Jake
    | 'T' -> Number 10
    | 'A' -> Number 1
    | '1' .. '9' as c -> Number (Mdrp_lib.Char.to_digit c)
    | c ->
        failwith
          (Format.sprintf "the character %c doesn't represent any card value" c)
end

module Suit = struct
  type t = Club | Diamond | Heart | Spade

  (* param is in [0..3] because it comes from i / 13  *)
  let of_int = function
    | 0 -> Club
    | 1 -> Diamond
    | 2 -> Heart
    | 3 -> Spade
    | n -> raise (NotInBound (n, (0, 3)))

  let of_char = function
    | 'C' -> Club
    | 'D' -> Diamond
    | 'H' -> Heart
    | 'S' -> Spade
    | c ->
        failwith
          (Format.sprintf "the character %c doesn't represent any card suit" c)

  let pp fmt c =
    Format.fprintf fmt "%s"
      (match c with
      | Club -> "♧"
      | Diamond -> "♢"
      | Heart -> "♡"
      | Spade -> "♤")

  let equal c1 c2 =
    match (c1, c2) with
    | Club, Club | Diamond, Diamond | Heart, Heart | Spade, Spade -> true
    | _ -> false

  let compare c1 c2 =
    if equal c1 c2 then 0
    else
      match (c1, c2) with
      | Club, _ -> 1
      | _, Club -> -1
      | Diamond, _ -> 1
      | _, Diamond -> -1
      | Heart, _ -> 1
      | _ -> -1
end

type t = { value : Value.t; suit : Suit.t }

let of_int i =
  if i < 0 || i > 51 then raise (NotInBound (i, (0, 51)))
  else { value = Value.of_int (i mod 13); suit = Suit.of_int (i / 13) }

let pp fmt { value; suit } =
  Format.fprintf fmt "%a%a" Value.pp value Suit.pp suit

let equal c1 c2 = Suit.equal c1.suit c2.suit && Value.equal c1.value c2.value

let compare c1 c2 =
  let c = Suit.compare c1.suit c2.suit in
  if c = 0 then Value.compare c1.value c2.value else c

let of_string s = { value = Value.of_char s.[0]; suit = Suit.of_char s.[1] }

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)
