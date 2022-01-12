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

let get_and_remove a i =
  let tmp = a.(i) in
  for k = i to Array.length a - 2 do
    a.(k) <- a.(k + 1)
  done;
  tmp

module Matrix = struct
  type 'a t = 'a array array

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

  let pp_array = pp

  let pp ?(pp_sep = fun ppf () -> Format.fprintf ppf "; ") ?(left = "[|")
      ?(right = "|]") pp ppf m =
    Format.fprintf ppf "@[<v 2>%a@]"
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

  let width_height m = (Array.length m.(0), Array.length m)
end
