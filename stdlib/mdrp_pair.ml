let rev (x, y) = (y, x)

let pp ?(left = "(") ?(right = ")") ?(sep = ", ") pp_fst pp_snd ppf (fst, snd) =
  Format.fprintf ppf "%s%a%s%a%s" left pp_fst fst sep pp_snd snd right

let pp_proj proj ppe ppf ((_fst, _snd) as pair) =
  Format.fprintf ppf "%a" ppe (proj pair)

module Make (X : sig
  include Mdrp_types.Printable
  include Mdrp_types.Comparable with type t := t
end) (Y : sig
  include Mdrp_types.Printable
  include Mdrp_types.Comparable with type t := t
end) =
struct
  type t = X.t * Y.t

  let compare (x1, y1) (x2, y2) =
    match X.compare x1 x2 with 0 -> Y.compare y1 y2 | c -> c

  let equal (x1, y1) (x2, y2) = X.compare x1 x2 = 0 && Y.compare y1 y2 = 0
  let pp ppf t = pp X.pp Y.pp ppf t
end
