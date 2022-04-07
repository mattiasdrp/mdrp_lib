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
end
