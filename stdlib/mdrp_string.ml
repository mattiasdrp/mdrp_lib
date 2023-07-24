module String = struct
  include Stdlib.String

  let pp ppf t = Format.fprintf ppf "%s" t

  let is_palindrome t =
    let l = length t in
    let rec comp n = n = 0 || (t.[l - n] = t.[n - 1] && comp (n - 1)) in
    comp (l / 2)

  let fold f acc t =
    let ls = length t in
    let rec aux i acc =
      if i = ls then acc else aux (i + 1) (f acc (unsafe_get t i))
    in
    aux 0 acc

  let foldi f acc t =
    let ls = length t in
    let rec aux i acc =
      if i = ls then acc else aux (i + 1) (f i acc (unsafe_get t i))
    in
    aux 0 acc

  let append_char t c = t ^ Char.escaped c
  let prepend_char t c = Char.escaped c ^ t

  let reverse t =
    let l = length t in
    init l (fun i -> t.[l - i - 1])

  let characters t =
    fold (fun acc c -> Mdrp_char.Set.add c acc) Mdrp_char.Set.empty t

  let slices t sub_length =
    let tlength = length t in
    let separations =
      (tlength / sub_length) + if tlength mod sub_length != 0 then 1 else 0
    in
    Seq.init separations (fun i ->
        let sub_length =
          if i = separations - 1 then tlength - (i * sub_length) else sub_length
        in
        sub t (i * sub_length) sub_length)

  let to_list t = fold (fun acc c -> c :: acc) [] t |> List.rev
  let to_array f t = Array.init (length t) (fun i -> f t.[i])
  let to_arrayi f t = Array.init (length t) (fun i -> f i t.[i])
  let delete_end t n = sub t 0 (length t - n)
  let delete_start t n = sub t n (length t - n)
  let to_int t = fold (fun acc c -> acc + Mdrp_char.order c) 0 t

  let split_on_char_non_empty sep t =
    let res = ref [] in
    let buf = Buffer.create 13 in
    for i = 0 to length t - 1 do
      let c = unsafe_get t i in
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

  let is_lowercase_ascii t =
    for_all (function 'a' .. 'z' -> true | _ -> false) t
end

module Set = Mdrp_set.Make (String)
module Map = Mdrp_map.Make (String)
include String
