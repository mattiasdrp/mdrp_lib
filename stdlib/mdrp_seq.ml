include Stdlib.Seq

let next_opt seq =
  match seq () with Nil -> (None, seq) | Cons (e, seq') -> (Some e, seq')

let next_split (prev, next) =
  match next with
  | [] -> None
  | hd :: tl -> Some ((hd, Mdrp_list.rev_append prev tl), (hd :: prev, tl))

let all_head_tails l = unfold next_split ([], l)

(* Get all the possible combinations leading to n *)
(* let combinations add zero l n =
 *   let rec next_combination (sum, comb) (prev, next) =
 *     assert (sum < n);
 *     match rest with
 *     | [] -> (\* sum < n et plus rien à faire, revenir avant *\)
 *     | hd :: tl ->
 *
 *   in
 *
 *   let rec aux ((sum, comb), res) rest =
 *     if sum = n then Some (comb :: res)
 *     else if sum > n then None
 *     else
 *       match rest with
 *       | [] -> None
 *       | hd :: tl ->
 *           let res1 = aux ((add hd sum, hd :: comb), res) rest in
 *           let res2 = aux ((sum, comb), res) tl in
 *           rev_append res1 res2
 *   in
 *   aux ((zero, []), []) l *)

type 'a state = {
  length : int;
  i : int;
  cpt : int array;
  array : 'a array;
  first : bool;
}

(* State machine for Heap's algorithm *)
let rec permut ({ length; i; cpt; array; first } as state) =
  if first then Some (Mdrp_array.to_list array, { state with first = false })
  else if i < length then
    if cpt.(i) < i then (
      if i mod 2 = 0 then Mdrp_array.swap array 0 i
      else Mdrp_array.swap array cpt.(i) i;
      cpt.(i) <- cpt.(i) + 1;
      Some (Mdrp_array.to_list array, { state with i = 0 }))
    else (
      cpt.(i) <- 0;
      permut { state with i = state.i + 1 })
  else None

let permutations l =
  let array = Mdrp_array.of_list l in
  let length = Mdrp_array.length array in
  let cpt = Mdrp_array.init length (fun _ -> 0) in
  unfold permut { length; i = 0; cpt; array; first = true }

let subsets k l =
  let a = Mdrp_array.of_list l in
  let n = Mdrp_array.length a in
  let length = Mdrp_array.length a in
  let rec aux_subsets (k', index, indices, picked) =
    if k' = k then
      (* We need to pick one last element *)
      if index = length then
        let index, indices, picked =
          match (indices, picked) with
          | i :: il, _ :: pl -> (i, il, pl)
          | _ -> assert false
        in
        aux_subsets (k' - 1, index + 1, indices, picked)
      else
        Some (List.rev (a.(index) :: picked), (k', index + 1, indices, picked))
    else if
      (* We can't add an element because there will be not enough
         elements left *)
      index > length - k'
    then
      let res =
        match (indices, picked) with
        | i :: il, _ :: pl -> Some (i, il, pl)
        | _ -> None
      in
      match res with
      | Some (index, indices, picked) ->
          aux_subsets (k' - 1, index + 1, indices, picked)
      | None -> None
    else aux_subsets (k' + 1, index + 1, index :: indices, a.(index) :: picked)
  in
  if k = 0 || k > n then Seq.empty else Seq.unfold aux_subsets (1, 0, [], [])
  let n = Mdrp_list.length l in
  let cpt = Mdrp_array.init n (fun _ -> 0) in
  unfold permut { n; i = 0; cpt; a = Mdrp_array.of_list l; first = true }

let next_split (prev, next) =
  match next with
  | [ _ ] | [] -> None
  | hd :: tl ->
      let prev = hd :: prev in
      Some ((Mdrp_list.rev prev, tl), (prev, tl))

let all_splits l = unfold next_split ([], l)

let find_opt f seq =
  let rec aux seq =
    match seq () with
    | Nil -> None
    | Cons (x, next) -> if f x then Some x else aux next
  in
  aux seq

let find_map f seq =
  let rec aux seq =
    match seq () with
    | Nil -> None
    | Cons (x, next) -> ( match f x with Some _ as r -> r | None -> aux next)
  in
  aux seq

let findi_opt f seq =
  let rec aux i seq =
    match seq () with
    | Nil -> None
    | Cons (x, next) -> if f i x then Some x else aux (i + 1) next
  in
  aux 0 seq

let findi_map f seq =
  let rec aux i seq =
    match seq () with
    | Nil -> None
    | Cons (x, next) -> (
        match f i x with Some _ as r -> r | None -> aux (i + 1) next)
  in
  aux 0 seq

let find_fold f acc seq =
  let rec aux acc seq =
    match seq () with
    | Nil -> None
    | Cons (x, next) ->
        let acc, b = f acc x in
        if b then Some (x, acc) else aux acc next
  in
  aux acc seq
