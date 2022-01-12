module Meters = struct
  type t = float

  let of_yards y = 0.9144 *. y
end
