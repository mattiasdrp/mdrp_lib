include Stdlib.Int

let pp ppf n = Format.fprintf ppf "%d" n

type sign = Negative | Positive | Zero

module Set = Mdrp_set.Make (Stdlib.Int)
module Map = Mdrp_map.Make (Stdlib.Int)
module Multiset = Mdrp_multiset.Make (Int)

module Decimal = struct
  (* Fast exponentiation *)
  type t = int

  type digit = int

  let pow x p =
    let rec aux p =
      if p = 0 then 1
      else if p = 1 then x
      else
        let xx = aux (p / 2) in
        if p mod 2 = 0 then xx * xx else x * xx * xx
    in
    if p < 0 then failwith "Failure: p < 0" else aux p

  let ( ** ) x p = pow x p

  (* Number of decimal digits *)
  let nb_digits n =
    let rec aux n acc = if n < 10 then acc else aux (n / 10) (acc + 1) in
    aux n 1

  let to_digits n =
    let rec aux n acc =
      let acc = (n mod 10) :: acc in
      if n < 10 then acc else aux (n / 10) acc
    in
    aux n []

  let of_digits l =
    let rec aux res = function
      | [] -> res
      | hd :: tl -> aux ((res * 10) + hd) tl
    in
    aux 0 l

  let reverse n = to_digits n |> List.rev |> of_digits

  let to_string n = to_string n

  let is_palindrome i = Mdrp_string.is_palindrome (to_string i)

  let of_string s = int_of_string s

  let sign i = if i < 0 then Negative else if i > 0 then Positive else Zero

  let sqrt n = truncate (sqrt (float n))

  let ( % ) i m = i mod m = 0

  let ( ! ) i =
    let rec aux i acc = if i = 0 || i = 1 then acc else aux (i - 1) (acc * i) in
    aux i 1

  let set_primes n =
    let a = Mdrp_array.init (n + 1) (fun _ -> true) in
    a.(0) <- false;
    a.(1) <- false;
    for p = 2 to sqrt n do
      if a.(p) = true then
        let rec aux i =
          if i <= n then (
            a.(i) <- false;
            aux (i + p))
        in
        aux (p * p)
    done;
    Mdrp_array.fold_lefti
      (fun i acc e -> if e then Set.add i acc else acc)
      Set.empty a

  let is_prime n =
    let lim = sqrt n in
    let rec aux d =
      if d > lim then true else if n mod d = 0 then false else aux (d + 2)
    in
    n = 2 || (n > 2 && n mod 2 = 1 && aux 3)

  external of_dec : int -> int = "%identity"

  let prime_factors i =
    if is_prime i then Multiset.add i (Multiset.singleton 1)
    else
      let rec aux acc i d =
        if i = 1 then acc
        else if i % d then aux (Multiset.add d acc) (i / d) d
        else aux acc i (d + 1)
      in
      aux Multiset.empty i 2

  let modpow base exp modulus =
    if nb_digits modulus * 2 > nb_digits max_int then
      Z.(to_int (powm (of_int base) (of_int exp) (of_int modulus)))
    else if modulus = 1 then 0
    else
      let rec aux base exp result =
        if exp = 0 then result
        else
          let result =
            if exp mod 2 = 1 then result * base mod modulus else result
          in
          aux (base * base mod modulus) (exp lsr 1) result
      in
      aux (base mod modulus) exp 1

  let same_digits t1 t2 =
    let l1 = to_digits t1 |> List.fast_sort compare in
    let l2 = to_digits t2 |> List.fast_sort compare in
    List.equal ( = ) l1 l2

  module Seq = struct
    let rec odd_composites_aux n =
      if is_prime n then odd_composites_aux (n + 2) else Some (n, n + 2)

    let odd_composites ?(start = 9) () = Seq.unfold odd_composites_aux start

    let rec primes_aux limit n =
      if n = 1 || n = 2 then Some (2, 3)
      else if limit > 0 && n > limit then None
      else if is_prime n then Some (n, n + 2)
      else primes_aux limit (n + 2)

    let primes ?(start = 2) ?(limit = -1) () =
      Seq.unfold (primes_aux limit)
        (if start = 2 then start else if start % 2 then start + 1 else start)

    let integers_aux limit n =
      if limit > 0 && n > limit then None else Some (n, n + 1)

    let integers ?(start = 0) ?(limit = -1) () =
      Seq.unfold (integers_aux limit) start

    let fibonacci_aux limit (fn1, fn2) =
      if limit > 0 && fn2 > limit then None
      else
        let fn = fn1 + fn2 in
        Some (fn2, (fn, fn1))

    let fibonacci ?(limit = -1) () = Seq.unfold (fibonacci_aux limit) (1, 1)

    let binomials_aux =
      let t = Hashtbl.create 19 in
      Hashtbl.add t (0, 0) Z.one;
      Hashtbl.add t (1, 0) Z.one;
      Hashtbl.add t (1, 1) Z.one;
      fun limit (n, r) ->
        let rec aux (n, r) =
          if limit > zero && n > limit then None
          else if r > n then aux (n + 1, 0)
          else if r = zero || r = n then (
            Hashtbl.add t (n, r) Z.one;
            Some (Z.one, (n, r + 1)))
          else if n > 1 then (
            let bc =
              Z.( + )
                (Hashtbl.find t (n - 1, r))
                (Hashtbl.find t (n - one, r - one))
            in
            Hashtbl.add t (n, r) bc;
            Some (bc, (n, r + one)))
          else if n = zero then Some (Z.one, (one, zero))
          else if n = one && r = zero then Some (Z.one, (one, one))
          else Some (Z.one, (2, 0))
        in

        aux (n, r)

    let binomials ?(start = 0) ?(limit = -1) () =
      Seq.unfold (binomials_aux limit) (start, 0)
  end
end

module Binary = struct
  (* Number of binary digits *)
  type t = string

  type digit = char

  let of_dec i =
    if i < 0 then invalid_arg "bin_of_dec"
    else if i = 0 then "0"
    else
      let rec aux acc d =
        if d = 0 then acc else aux (string_of_int (d land 1) ^ acc) (d lsr 1)
      in
      aux "" i

  let nb_digits = String.length

  let of_digits l =
    let rec aux res = function
      | [] -> res
      | hd :: tl -> aux (Format.sprintf "%c%s" hd res) tl
    in
    aux "" l

  let to_string n = n

  let of_string s = s

  let sign _ = Positive

  let pow _ = failwith "Not implemented"

  let to_digits t = List.of_seq @@ String.to_seq t
end

module Z = struct
  include Z

  let to_digits n =
    let ten = of_int 10 in
    let rec aux n acc =
      let acc = (n mod ten) :: acc in
      if n < ten then acc else aux (n / ten) acc
    in
    aux n []

  let nb_digits n =
    let ten = of_int 10 in
    let rec aux n acc = if n < ten then acc else aux (n / ten) (acc + one) in
    aux n one
end
