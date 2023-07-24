module type S = sig
  include Map.S

  val add_list : (key * 'a) list -> 'a t -> 'a t
  val of_list : (key * 'a) list -> 'a t

  val pp :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    ?left:string ->
    ?right:string ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit

  val find_predicate : (key -> 'a -> bool) -> 'a t -> key * 'a
  (** [find_predicate f t] returns the first [(key, value)] pair such that [f key value] is [true]. *)
end

module Make (X : sig
  include Mdrp_types.Printable
  include Mdrp_types.Comparable with type t := t
end) =
struct
  include Map.Make (X)

  let add_list l t = List.fold_left (fun acc (e, v) -> add e v acc) t l
  let of_list l = add_list l empty

  let pp ?(pp_sep = Format.pp_print_cut) ?(left = "{") ?(right = "}") ppv ppf t
      =
    Mdrp_list.(
      pp ~pp_sep ~left ~right
        Mdrp_pair.(pp ~left:"" ~right:"" ~sep:": " X.pp ppv))
      ppf (bindings t)

  let find_predicate (type a) f (t : a t) =
    let exception Found of (key * a) in
    try
      iter (fun k v -> if f k v then raise (Found (k, v))) t;
      raise Not_found
    with Found res -> res
end
