module String = struct
  include Stdlib.String

  let pp ppf s = Format.fprintf ppf "%s" s

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

  let foldi f acc s =
    let ls = length s in
    let rec aux i acc =
      if i = ls then acc else aux (i + 1) (f i acc (unsafe_get s i))
    in
    aux 0 acc

  let characters s =
    fold (fun acc c -> Mdrp_char.Set.add c acc) Mdrp_char.Set.empty s

  let to_list s = fold (fun acc c -> c :: acc) [] s |> List.rev
  let to_array f s = Array.init (String.length s) (fun i -> f s.[i])
  let to_arrayi f s = Array.init (String.length s) (fun i -> f i s.[i])
  let delete_end s n = String.sub s 0 (String.length s - n)
  let delete_start s n = String.sub s n (String.length s - n)
  let to_int s = fold (fun acc c -> acc + Mdrp_char.order c) 0 s

  let split_on_char_non_empty sep s =
    let res = ref [] in
    let buf = Buffer.create 13 in
    for i = 0 to length s - 1 do
      let c = unsafe_get s i in
      if c = sep then
        if Buffer.length buf > 0 then (
          res := Buffer.contents buf :: !res;
          Buffer.clear buf)
        else ()
      else Buffer.add_char buf c
    done;
    let res =
      if Buffer.length buf > 0 then Buffer.contents buf :: !res else !res
    in
    List.rev res

  let is_lowercase_ascii s =
    for_all (function 'a' .. 'z' -> true | _ -> false) s
end

module Set = Mdrp_set.Make (String)
module Map = Mdrp_map.Make (String)
include String
