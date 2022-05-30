type 'a t = { offset : int; array : 'a Array.t }

let init ~min_bound ~max_bound f =
  let max_bound = max_bound + 1 in
  {
    offset = -min_bound;
    array = Array.init (max_bound - min_bound) (fun i -> f (i + min_bound));
  }

let of_array min_bound array = { offset = -min_bound; array }
let length { array; _ } = Array.length array
let get t i = Array.get t.array (i + t.offset)
let ( .%() ) t i = get t i
let set t i v = Array.set t.array (i + t.offset) v
let ( .%()<- ) t i v = set t i v
