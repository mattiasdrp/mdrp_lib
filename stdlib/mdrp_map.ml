module type S = sig
  include Map.S

  val add_list : (key * 'a) list -> 'a t -> 'a t
  val of_list : (key * 'a) list -> 'a t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Make (X : sig
  include Mdrp_types.Printable
  include Mdrp_types.Comparable with type t := t
end) =
struct
  include Map.Make (X)

  let add_list l t = List.fold_left (fun acc (e, v) -> add e v acc) t l
  let of_list l = add_list l empty

  let pp pp_v ppf t =
    Format.fprintf ppf "@[<v 0>%a@]"
      Mdrp_list.(
        pp ~left:"" ~right:"" ~pp_sep:Format.pp_print_cut
          Mdrp_pair.(pp ~left:"" ~right:"" ~sep:": " X.pp pp_v))
      (bindings t)
  (* iter (fun k v -> Format.fprintf ppf "@[<v 2>%a:@,%a@]@," X.pp k pp_v v) t *)
end
