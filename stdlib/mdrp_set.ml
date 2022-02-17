module type S = sig
  include Set.S

  val add_list : elt list -> t -> t
  val of_list : elt list -> t

  val pp :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    ?left:string ->
    ?right:string ->
    (Format.formatter -> elt -> unit) ->
    Format.formatter ->
    t ->
    unit
end

module Make (Ord : Set.OrderedType) = struct
  include Set.Make (Ord)

  let add_list l t = List.fold_left (fun acc e -> add e acc) t l
  let of_list l = add_list l empty

  let pp ?(pp_sep = fun ppf () -> Format.fprintf ppf "; ") ?(left = "[")
      ?(right = "]") ppe ppf t =
    Mdrp_list.(pp ~pp_sep ~left ~right ppe) ppf (elements t)
end
