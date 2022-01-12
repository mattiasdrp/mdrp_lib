module Set = struct
  include Set.Make (Int)

  let add_list l t = List.fold_left (fun acc e -> add e acc) t l
end

module Decimal = struct
  (* Fast exponentiation *)
  let pow x p =
    let rec aux p =
      if p = 0 then 1
      else if p = 1 then x
      else
        let xx = aux (p / 2) in
        if p mod 2 = 0 then xx * xx else x * xx * xx
    in
    if p < 0 then failwith "Failure: p < 0" else aux p

  (* Number of decimal digits *)
  let nb_digits n =
    let rec aux n acc = if n < 10 then acc else aux (n / 10) (acc + 1) in
    aux n 1

  let to_digits n =
    let rec aux n acc =
      if n < 10 then acc else aux (n / 10) ((n mod 10) :: acc)
    in
    aux n []

  let from_digits l =
    let rec aux res = function
      | [] -> res
      | hd :: tl -> aux ((res * 10) + hd) tl
    in
    aux 0 l
end

module Binary = struct
  (* Number of binary digits *)
  let nb_digits n =
    let rec aux n acc = if n < 2 then acc else aux (n / 2) (acc + 1) in
    aux n 1

  let from_digits l =
    let rec aux res = function
      | [] -> res
      | hd :: tl ->
          assert (hd < 2);
          aux ((res * 2) + hd) tl
    in
    aux 0 l
end
