include Stdlib.Option

let pp pp ppf = function
  | None -> Format.fprintf ppf "None"
  | Some v -> Format.fprintf ppf "Some %a" pp v

let pp_int ppf = pp Mdrp_int.pp ppf

let pp_string ppf = pp Mdrp_string.pp ppf

module Value = struct
  let pp pp ppf = function
    | None -> assert false
    | Some v -> Format.fprintf ppf "%a" pp v

  let pp_int ppf = pp Mdrp_int.pp ppf

  let pp_string ppf = pp Mdrp_string.pp ppf
end
