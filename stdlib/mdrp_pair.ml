let pp pp_fst pp_snd ppf (fst, snd) =
  Format.fprintf ppf "(%a, %a)" pp_fst fst pp_snd snd

let pp_proj proj ppe ppf ((_fst, _snd) as pair) =
  Format.fprintf ppf "%a" ppe (proj pair)
