include Stdlib.Array

let pp ?(pp_sep = fun ppf () -> Format.fprintf ppf "; ") ?(left = "[|")
    ?(right = "|]") pp ppf a =
  Mdrp_list.pp ~pp_sep ~left ~right pp ppf (Array.to_list a)

let ( .?() ) a i = try Some (Array.get a i) with Invalid_argument _ -> None

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

let iteri f a =
  for i = 0 to length a - 1 do
    f i (unsafe_get a i)
  done

let get_and_remove a i =
  let tmp = a.(i) in
  for k = i to Array.length a - 2 do
    a.(k) <- a.(k + 1)
  done;
  tmp

let[@inline] out_of_bounds a i = i < 0 || i >= Array.length a

module Matrix = struct
  type 'a t = 'a array array

  let[@inline] out_of_bounds m i j =
    i < 0 || j < 0 || i >= Array.length m || j >= Array.length m.(0)

  let moore_neighbourhood ?(keep_centre = false) m i j =
    let maxi = i + 1 in
    let maxj = j + 1 in
    let height = Array.length m in
    let width = Array.length m.(0) in

    let rec moore_neighbourhood_aux (ni, nj) =
      if ni = height || ni > maxi then None
      else if ni < 0 || nj > maxj || nj = width then
        moore_neighbourhood_aux (ni + 1, j - 1)
      else if nj < 0 || ((not keep_centre) && ni = i && nj = j) then
        moore_neighbourhood_aux (ni, nj + 1)
      else Some ((ni, nj), (ni, nj + 1))
    in
    Seq.unfold moore_neighbourhood_aux (i - 1, j - 1)

  let neumann_neighbourhood m i j =
    let l = [ (i, j + 1); (i + 1, j); (i, j - 1); (i - 1, j) ] in
    let width = Array.length m.(0) in
    let height = Array.length m in
    let rec neumann_neighbourhood_aux = function
      | [] -> None
      | (i, j) :: tl when i < 0 || i = height || j < 0 || j = width ->
          neumann_neighbourhood_aux tl
      | c :: tl -> Some (c, tl)
    in

    Seq.unfold neumann_neighbourhood_aux l

  let queen_move =
    let dirs =
      [ (-1, -1); (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1) ]
    in
    fun p m i j ->
      let rec all_directions_aux (ni, nj, di, dj, dirs) =
        let ni, nj = (ni + di, nj + dj) in
        if out_of_bounds m ni nj then
          match dirs with
          | [] -> None
          | (di, dj) :: tl -> all_directions_aux (i, j, di, dj, tl)
        else if p m ni nj then all_directions_aux (ni, nj, di, dj, dirs)
        else
          match dirs with
          | [] -> Some ((ni, nj), (-1, -1, 0, 0, []))
          | (di, dj) :: tl -> Some ((ni, nj), (i, j, di, dj, tl))
      in

      Seq.unfold all_directions_aux (i, j, -1, 0, dirs)

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

  let fold f acc m =
    fold_lefti
      (fun i acc v -> fold_lefti (fun j acc v -> f acc i j v) acc v)
      acc m

  let iter f m = iteri (fun i v -> iteri (fun j v -> f i j v) v) m
  let width_height m = (Array.length m.(0), Array.length m)
end
