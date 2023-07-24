include Stdlib.List

let pp ?(pp_sep = fun ppf () -> Format.fprintf ppf "; ") ?(left = "[")
    ?(right = "]") pp ppf l =
  Format.fprintf ppf "%s%a%s" left Format.(pp_print_list ~pp_sep pp) l right

(* let ( @? ) v l = match v with Some v -> v :: l | None -> l *)

let combinations ~add ~zero l n =
  let rec aux ((sum, comb), res) rest =
    if sum = n then comb :: res
    else if sum > n then res
    else
      match rest with
      | [] -> res
      | hd :: tl ->
          let res1 = aux ((add hd sum, hd :: comb), res) rest in
          let res2 = aux ((sum, comb), res) tl in
          rev_append res1 res2
  in
  aux ((zero, []), []) l

let swap a i j =
  let tmp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tmp

let permut n a =
  let cpt = Array.init n (fun _ -> 0) in
  let r = ref [ Array.to_list a ] in
  let i = ref 0 in
  while !i < n do
    let di = !i in
    if cpt.(di) < di then (
      if di mod 2 = 0 then swap a 0 di else swap a cpt.(di) di;
      r := Array.to_list a :: !r;
      cpt.(di) <- cpt.(di) + 1;
      i := 0)
    else (
      cpt.(di) <- 0;
      incr i)
  done;
  !r

let permutations l =
  let a = Array.of_list l in
  permut (Array.length a) a

let rotations l =
  let rec aux acc lr =
    match lr with
    | [] -> assert false
    | hd :: tl ->
        let lr = tl @ [ hd ] in
        if lr = l then acc else aux (lr :: acc) lr
  in
  aux [ l ] l
(* [1; 2; 3]
 * [2; 3; 1]
 * [3; 1; 2]*)

let take n l =
  let rec aux i acc = function
    | [] -> acc
    | _ when i = 0 -> acc
    | hd :: tl -> aux (i - 1) (hd :: acc) tl
  in
  List.rev (aux n [] l)

let cartesian_product l1 l2 =
  let rec aux acc = function
    | [] -> acc
    | hd1 :: tl -> aux (fold_left (fun acc hd2 -> (hd1, hd2) :: acc) acc l2) tl
  in
  aux [] l1

let fold_lefti f acc l =
  snd (List.fold_left (fun (i, acc) v -> (i + 1, f acc i v)) (0, acc) l)
