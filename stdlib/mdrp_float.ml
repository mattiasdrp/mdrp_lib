include Stdlib.Float

let pp ppf t = Format.fprintf ppf "%.2f" t
let pp_percent ppf t = Format.fprintf ppf "%.2f%%" (100. *. t)
