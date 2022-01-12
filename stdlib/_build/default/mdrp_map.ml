module Make (Ord : Map.OrderedType) = struct
  include Map.Make (Ord)

  let add_list l t = List.fold_left (fun acc (e, v) -> add e v acc) t l

  let of_list l = add_list l empty
end
