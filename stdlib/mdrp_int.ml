include Stdlib.Int

let pp ppf n = Format.fprintf ppf "%d" n

type sign = Negative | Positive | Zero

let compare_dec i1 i2 = compare i2 i1

module List = struct
  let pp' = pp
  let compare' = compare

  include Mdrp_list

  let pp ppf t = Mdrp_list.pp pp' ppf t
  let compare t1 t2 = Mdrp_list.compare compare' t1 t2

  type t = int list
end

module Set = struct
  include Mdrp_set.Make (struct
    include Stdlib.Int

    let pp = pp
  end)
end

module PairSet = struct
  include Mdrp_set.Make (struct
    type t = int * int

    let compare (a1, a2) (b1, b2) =
      let c = compare a1 b1 in
      if c = 0 then compare a2 b2 else c

    let pp = Mdrp_pair.pp pp pp
  end)
end

let primes =
  [
    2;
    3;
    5;
    7;
    11;
    13;
    17;
    19;
    23;
    29;
    31;
    37;
    41;
    43;
    47;
    53;
    59;
    61;
    67;
    71;
    73;
    79;
    83;
    89;
    97;
    101;
    103;
    107;
    109;
    113;
    127;
    131;
    137;
    139;
    149;
    151;
    157;
    163;
    167;
    173;
    179;
    181;
    191;
    193;
    197;
    199;
    211;
    223;
    227;
    229;
    233;
    239;
    241;
    251;
    257;
    263;
    269;
    271;
    277;
    281;
    283;
    293;
    307;
    311;
    313;
    317;
    331;
    337;
    347;
    349;
    353;
    359;
    367;
    373;
    379;
    383;
    389;
    397;
    401;
    409;
    419;
    421;
    431;
    433;
    439;
    443;
    449;
    457;
    461;
    463;
    467;
    479;
    487;
    491;
    499;
    503;
    509;
    521;
    523;
    541;
  ]
  |> Set.of_list

module Map = Mdrp_map.Make (struct
  include Stdlib.Int

  let pp ppf d = Format.fprintf ppf "%d" d
end)

module PairMap = Mdrp_map.Make (struct
  type t = int * int

  let compare (a1, a2) (b1, b2) =
    let c = compare a1 b1 in
    if c = 0 then compare a2 b2 else c

  let pp ppf (d1, d2) = Format.fprintf ppf "(%d, %d)" d1 d2
end)

module Multiset = Mdrp_multiset.Make (struct
  include Stdlib.Int

  let pp ppf d = Format.fprintf ppf "%d" d
end)

let swap_max k v ((_, vmax) as acc) = if v > vmax then (k, v) else acc
let swap_min k v ((_, vmin) as acc) = if v < vmin then (k, v) else acc

module Decimal = struct
  (* Fast exponentiation *)
  type t = int
  type digit = int

  (* Number of decimal digits *)
  let nb_digits n =
    let rec aux n acc = if n < 10 then acc else aux (n / 10) (acc + 1) in
    aux n 1

  let log n = log (float n)

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

  let gcd a b =
    let rec aux a b = if b = 0 then a else aux b (a mod b) in
    aux (max a b) (min a b)

  (** [extended_gcd a b = r, u, v] such that [a.u + b.v = r] *)
  let extended_gcd a b =
    let rec aux r1 r2 u1 u2 v1 v2 =
      if r2 = 0 then (r1, u1, v1)
      else
        let q = r1 / r2 in
        aux r2 (r1 - (q * r2)) u2 (u1 - (q * u2)) v2 (v1 - (q * v2))
    in
    aux a b 0 1 1 0

  let lcm a b = abs a * (abs b / gcd a b)

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
  let of_string s = int_of_string s
  let concatenate n1 n2 = of_string (to_string n1 ^ to_string n2)
  let is_palindrome i = Mdrp_string.is_palindrome (to_string i)
  let sign i = if i < 0 then Negative else if i > 0 then Positive else Zero
  let sqrt n = truncate (sqrt (float n))

  let is_square x =
    let sx = sqrt x in
    sx * sx = x

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

  let is_prime =
    let ht = Hashtbl.create 19 in
    fun n ->
      Set.mem n primes || Hashtbl.mem ht n
      ||
      let lim = sqrt n in
      let rec aux d =
        if d > lim then true else if n mod d = 0 then false else aux (d + 2)
      in
      n = 2
      || n > 2
         && n mod 2 = 1
         &&
         let res = aux 3 in
         if res then Hashtbl.add ht n ();
         res

  external of_dec : int -> int = "%identity"

  let to_bin ?extend t =
    if t < 0 then invalid_arg "bin_of_dec"
    else
      let t =
        if t = 0 then "0"
        else
          let rec aux acc d =
            if d = 0 then acc else aux (string_of_int (d land 1) ^ acc) (d lsr 1)
          in
          aux "" t
      in
      match extend with
      | None -> t
      | Some length ->
          Printf.sprintf "%*s" length t
          |> String.map (function ' ' -> '0' | c -> c)

  let of_bin b =
    String.fold_right
      (fun c (pow2, res) ->
        let res =
          if c = '0' then res
          else if c = '1' then res + pow2
          else failwith "invalid bit"
        in
        (2 * pow2, res))
      b (1, 0)
    |> snd

  let divisors n =
    let sn = sqrt n in
    let rec aux i acc =
      if i > sn then acc
      else
        let acc =
          if n mod i = 0 then Set.add i (Set.add (n / i) acc) else acc
        in
        aux (i + 1) acc
    in
    aux 1 Set.empty

  let prime_divisors i =
    if is_prime i then Multiset.singleton i
    else
      let rec aux acc i d =
        if i = 1 then acc
        else if i % d then aux (Multiset.add d acc) (i / d) d
        else aux acc i (d + 1)
      in
      aux Multiset.empty i 2

  let nb_divisors n =
    let rec aux acc i d =
      if i = 1 then acc
      else if i % d then (
        Format.eprintf "%d %% %d@." i d;
        aux (acc + 1) (i / d) d)
      else aux acc i (d + 1)
    in
    aux 1 n 2

  let sum_divisors n =
    let divs = prime_divisors n in
    Multiset.fold (fun d p acc -> acc * ((pow d (p + 1) - 1) / (d - 1))) divs 1

  let same_digits t1 t2 =
    let l1 = to_digits t1 |> List.fast_sort compare in
    let l2 = to_digits t2 |> List.fast_sort compare in
    List.equal ( = ) l1 l2

  module Modular_Arithmetic = struct
    let add a b m = ((a mod m) + (b mod m)) mod m
    let mul a b m = a mod m * (b mod m) mod m
    let x a b c = a + (b * a) + c

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

    let inverse a m =
      let r, u, _ = extended_gcd m a in
      assert (r = 1);
      u

    (** l is composed of (a, n) such that t = a [n] *)
    let chinese_remainder l =
      let big_n = List.fold_left (fun acc (_, n) -> n * acc) 1 l in
      let res =
        List.fold_left
          (fun acc (a, n) ->
            let y = big_n / n in
            let z = inverse y n in
            add acc (a * y * z) big_n)
          0 l
      in
      if res < 0 then res + big_n else res
  end

  module Seq = struct
    let rec odd_composites_aux n =
      if is_prime n then odd_composites_aux (n + 2) else Some (n, n + 2)

    let odd_composites ?(start = 9) () = Seq.unfold odd_composites_aux start

    let range min max =
      let aux_range n = if n > max then None else Some (n, n + 1) in
      Seq.unfold aux_range min

    (* Optimised with an array of only odd integers
       *)
    let primes_sieve_aux limit =
      let limit = (limit + 1) / 2 in
      let a = Mdrp_array.init limit (fun _ -> true) in
      a.(0) <- false;
      let sieve p =
        let rec aux i =
          if i < limit then (
            a.(i) <- false;
            aux (i + ((2 * p) + 1)))
        in
        aux (2 * p * (p + 1))
      in

      let rec aux n =
        if n = 0 then Some (2, 1)
        else if n >= limit then None
        else if a.(n) = true then (
          sieve n;
          Some ((2 * n) + 1, n + 1))
        else aux (n + 1)
      in
      aux

    let rec primes_aux limit n =
      if n = 1 || n = 2 then Some (2, 3)
      else if limit > 0 && n > limit then None
      else if is_prime n then Some (n, n + 2)
      else primes_aux limit (n + 2)

    let primes ?start ?(limit = -1) () =
      if limit > 0 then
        let start = match start with Some s -> s | None -> 0 in
        Seq.unfold (primes_sieve_aux limit) start
      else
        let start = match start with Some s -> s | None -> 2 in
        Seq.unfold (primes_aux limit)
          (if start = 2 then start else if start % 2 then start + 1 else start)

    let integers_aux limit succ n =
      if limit > 0 && n > limit then None else Some (n, succ n)

    let integers ?(start = 0) ?(limit = -1) ?(succ = succ) () =
      Seq.unfold (integers_aux limit succ) start

    let digits_aux n =
      if n = -1 then None
      else if n < 10 then Some (n mod 10, -1)
      else Some (n mod 10, n / 10)

    let digits n = Seq.unfold digits_aux n

    let digits_lr_aux s n =
      if n = String.length s then None
      else Some (Mdrp_char.to_digit s.[n], n + 1)

    let digits_lr n = Seq.unfold (digits_lr_aux (Int.to_string n)) 0

    let multiples_aux m limit n =
      if limit > 0 && n > limit then None else Some (n, n + m)

    let multiples ?(start = 0) ?(limit = -1) m =
      Seq.unfold (multiples_aux m limit) start

    let powers_aux base limit (currpow, exponent) =
      if exponent > limit then None
      else Some ((currpow, exponent), (currpow * base, exponent + 1))

    let powers ?(limit = -1) ?(exponent = 1) base =
      Seq.unfold (powers_aux base limit) (base, exponent)

    let divisors n =
      let sn = sqrt n in
      let rec divisors_aux (d, store) =
        if d > sn then None
        else
          match store with
          | Some ds -> Some (ds, (d, None))
          | None ->
              if n mod d = 0 then Some (d, (d + 1, Some (n / d)))
              else divisors_aux (d + 1, store)
      in
      Seq.unfold divisors_aux (1, None)

    let pair_divisors n =
      let sn = sqrt n in
      let rec divisors_aux d =
        if d > sn then None
        else if n mod d = 0 then Some ((d, n / d), d + 1)
        else divisors_aux (d + 1)
      in
      Seq.unfold divisors_aux 1

    let triangles_aux limit (acc, n) =
      if limit > 0 && n > limit then None
      else
        let acc = acc + n in
        Some (acc, (acc, n + 1))

    let triangles ?(start = 1) ?(limit = -1) () =
      let acc = if start > 1 then start * (start + 1) / 2 else 0 in
      Seq.unfold (triangles_aux limit) (acc, start)

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

    (* TODO: Try to make it a Seq *)
    let restricted_weak_composition ~n ~k ~min_value ~max_value =
      let rec aux (n : int) (k : int) : int list list =
        if k = 1 then [ [ n ] ]
        else
          Seq.fold_left
            (fun acc v ->
              List.rev_append
                (List.map (fun rest -> v :: rest) (aux (n - v) (k - 1)))
                acc)
            []
            (range min_value (min n max_value))
      in
      aux n k
  end
end

module Binary = struct
  (* Number of binary digits *)
  type t = string
  type digit = char

  let length = String.length
  let pp ppf s = Format.fprintf ppf "%s" s
  let nb_digits = String.length
  let to_digits t = List.of_seq @@ String.to_seq t

  let of_digits l =
    let rec aux res = function
      | [] -> res
      | hd :: tl -> aux (Format.sprintf "%c%s" hd res) tl
    in
    aux "" l

  let of_dec ?extend d = Decimal.to_bin ?extend d
  let to_dec t = Decimal.of_bin t
  let to_string t = t
  let of_string s = s
  let to_bytes t = Bytes.of_string t
  let of_bytes b = Bytes.to_string b
  let sign _ = Positive
  let pow _ = failwith "Not implemented"
end

module Z = struct
  include Z

  let minus_one = Z.of_int (-1)
  let pow x p = pow (of_int x) p

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

  let ( ! ) i = Z.(fac i)

  module Seq = struct
    let digits_aux (n : Z.t) =
      let ten = of_int 10 in
      if n = minus_one then None
      else if n < ten then Some (to_int (n mod ten), minus_one)
      else Some (to_int (n mod ten), n / ten)

    let digits n = Seq.unfold digits_aux n

    let digits_lr_aux s (n : Decimal.t) =
      if n = String.length s then None
      else Some (Mdrp_char.to_digit s.[n], Stdlib.(n + 1))

    let digits_lr n = Seq.unfold (digits_lr_aux (to_string n)) 0
  end
end
