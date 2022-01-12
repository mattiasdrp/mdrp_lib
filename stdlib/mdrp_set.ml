module Make (Ord : Set.OrderedType) = struct
  include Set.Make (Ord)

  let add_list l t = List.fold_left (fun acc e -> add e acc) t l

  let of_list l = add_list l empty

  let pp ?(pp_sep = fun ppf () -> Format.fprintf ppf "; ") ?(left = "[")
      ?(right = "]") ppe ppf t =
    Mdrp_list.(pp ~pp_sep ~left ~right ppe) ppf (elements t)
end
