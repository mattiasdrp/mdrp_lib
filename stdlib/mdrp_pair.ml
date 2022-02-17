let pp ?(left = "(") ?(right = ")") ?(sep = ", ") pp_fst pp_snd ppf (fst, snd) =
  Format.fprintf ppf "%s%a%s%a%s" left pp_fst fst sep pp_snd snd right

let pp_proj proj ppe ppf ((_fst, _snd) as pair) =
  Format.fprintf ppf "%a" ppe (proj pair)
