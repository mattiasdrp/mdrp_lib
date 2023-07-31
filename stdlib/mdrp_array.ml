include Stdlib.Array

let pp ?(pp_sep = fun ppf () -> Format.fprintf ppf "; ") ?(left = "[|")
    ?(right = "|]") pp ppf a =
  Mdrp_list.pp ~pp_sep ~left ~right pp ppf (Array.to_list a)

let compare cmp t1 t2 =
  let l1 = length t1 in
  let l2 = length t2 in
  let c = Stdlib.Int.compare l1 l2 in
  if c <> 0 then c
  else
    let rec aux i =
      if i = l1 then 0
      else
        let c = cmp (unsafe_get t1 i) (unsafe_get t2 i) in
        if c <> 0 then c else aux (i + 1)
    in
    aux 0

let equal eq t1 t2 =
  let l1 = length t1 in
  let l2 = length t2 in
  let rec aux i =
    if i = l1 then true
    else eq (unsafe_get t1 i) (unsafe_get t2 i) && aux (i + 1)
  in
  Stdlib.Int.equal l1 l2 && aux 0

let swap a i j =
  let tmp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tmp

let fold_lefti f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f i !r (unsafe_get a i)
  done;
  !r

let get_and_remove a i =
  let tmp = a.(i) in
  for k = i to Array.length a - 2 do
    unsafe_set a k (unsafe_get a (k + 1))
  done;
  tmp

let[@inline] out_of_bounds a i = i < 0 || i >= Array.length a

module Matrix = struct
  type 'a t = 'a array array

  let[@inline] out_of_bounds m ~row ~col =
    col < 0 || row < 0 || col >= Array.length m.(0) || row >= Array.length m

  let moore_neighbourhood ?(keep_centre = false) m ~row ~col =
    let max_col = col + 1 in
    let max_row = row + 1 in
    let height = Array.length m in
    let width = Array.length m.(0) in

    let rec moore_neighbourhood_aux (n_row, n_col) =
      if n_row = height || n_row > max_row then None
      else if n_row < 0 || n_col > max_col || n_col = width then
        moore_neighbourhood_aux (n_row + 1, col - 1)
      else if n_col < 0 || ((not keep_centre) && n_row = row && n_col = col)
      then moore_neighbourhood_aux (n_row, n_col + 1)
      else Some ((n_row, n_col), (n_row, n_col + 1))
    in
    Seq.unfold moore_neighbourhood_aux (row - 1, col - 1)

  let neumann_neighbourhood ?(keep_centre = false) m ~row ~col =
    let l =
      [ (row, col - 1); (row, col + 1); (row - 1, col); (row + 1, col) ]
    in
    let l = if keep_centre then (row, col) :: l else l in
    let rec neumann_neighbourhood_aux = function
      | [] -> None
      | (row, col) :: tl when out_of_bounds m ~row ~col ->
          neumann_neighbourhood_aux tl
      | c :: tl -> Some (c, tl)
    in

    Seq.unfold neumann_neighbourhood_aux l

  let queen_move =
    let dirs =
      [ (-1, -1); (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1) ]
    in
    fun predicate m ~row ~col ->
      let rec all_directions_aux (n_row, n_col, d_row, d_col, dirs) =
        let n_row, n_col = (n_row + d_row, n_col + d_col) in
        if out_of_bounds m ~row:n_row ~col:n_col then
          match dirs with
          | [] -> None
          | (d_row, d_col) :: tl ->
              all_directions_aux (row, col, d_row, d_col, tl)
        else if not (predicate m ~row:n_row ~col:n_col) then
          all_directions_aux (n_row, n_col, d_row, d_col, dirs)
        else
          match dirs with
          (* No more cells to find except the current one *)
          | [] -> Some ((n_row, n_col), (-1, -1, 0, 0, []))
          (* Return the current cell and move to the next direction *)
          | (d_col, d_row) :: tl ->
              Some ((n_row, n_col), (row, col, d_col, d_row, tl))
      in
      Seq.unfold all_directions_aux (row, col, -1, 0, dirs)

  let pp_array = pp

  let pp ?(pp_sep = fun ppf () -> Format.fprintf ppf "; ") ?(left = "[|")
      ?(right = "|]") pp ppf m =
    Format.fprintf ppf "@[<v %d>%a@]" (String.length left)
      (pp_array ~pp_sep:Format.pp_print_cut ~left ~right
         (pp_array ~pp_sep ~left ~right pp))
      m

  let pp_short pp ppf t =
    Format.fprintf ppf "@[<v 0>";
    Array.iter
      (fun a ->
        Array.iter (Format.fprintf ppf "%a" pp) a;
        Format.fprintf ppf "@,")
      t;
    Format.fprintf ppf "@]"

  let fold_left f acc m =
    fold_left (fun acc v -> fold_left (fun acc v -> f acc v) acc v) acc m

  let fold_lefti f acc m =
    fold_lefti
      (fun row acc v -> fold_lefti (fun col acc v -> f acc ~row ~col v) acc v)
      acc m

  let iter f m = iter (fun v -> iter (fun v -> f v) v) m
  let iteri f m = iteri (fun row v -> iteri (fun col v -> f ~row ~col v) v) m
  let rows_columns m = (Array.length m, Array.length m.(0))
end
