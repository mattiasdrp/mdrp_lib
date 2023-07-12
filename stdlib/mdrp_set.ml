module type S = sig
  include Set.S

  val add_list : elt list -> t -> t
  val of_list : elt list -> t

  val pp :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    ?left:string ->
    ?right:string ->
    unit ->
    Format.formatter ->
    t ->
    unit
end

module Make (X : sig
  include Mdrp_types.Printable
  include Mdrp_types.Comparable with type t := t
end) =
struct
  include Set.Make (X)

  let add_list l t = List.fold_left (fun acc e -> add e acc) t l
  let of_list l = add_list l empty

  let pp ?(pp_sep = fun ppf () -> Format.fprintf ppf "; ") ?(left = "[")
      ?(right = "]") () ppf t =
    Mdrp_list.(pp ~pp_sep ~left ~right X.pp) ppf (elements t)
end
