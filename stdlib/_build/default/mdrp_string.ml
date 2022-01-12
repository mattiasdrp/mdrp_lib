include Stdlib.String
module Set = Set.Make (String)

let is_palindrome s =
  let l = length s in
  let rec comp n = n = 0 || (s.[l - n] = s.[n - 1] && comp (n - 1)) in
  comp (l / 2)

let fold f acc s =
  let ls = length s in
  let rec aux i acc =
    if i = ls then acc else aux (i + 1) (f acc (unsafe_get s i))
  in
  aux 0 acc

let characters s =
  fold (fun acc c -> Mdrp_char.Set.add c acc) Mdrp_char.Set.empty s

let pp ppf s = Format.fprintf ppf "%s" s

let delete_end s n = String.sub s 0 (String.length s - n)

let delete_start s n = String.sub s n (String.length s - n)
