include Stdlib.Bool

let pp ppf b = Format.fprintf ppf "%b" b

module Prelude = struct
  let ( ||! ) a b = (a && not b) || (b && not a)
end
