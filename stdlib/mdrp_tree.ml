module Make (X : Stdlib.Set.OrderedType) = struct
  type elt = X.t

  type t = { size : int; tree : tree }

  and tree = Empty | Node of t * (elt * int) * t

  let empty = { size = 0; tree = Empty }

  let rec add x t =
    match t.tree with
    | Empty -> { size = 1; tree = Node (empty, (x, 1), empty) }
    | Node (l, (v, cpt), r) ->
        let c = X.compare x v in
        let tree =
          if c = 0 then Node (l, (v, cpt + 1), r)
          else if c < 0 then Node (add x l, (v, cpt), r)
          else Node (l, (v, cpt), add x r)
        in
        { size = t.size + 1; tree }

  let mediane t =
    match t.tree with
    | Empty -> None
    | Node (l, (_, cpt), r) ->
        let med = (l.size + cpt + r.size) / 2 in
        let rec aux med t =
          match t.tree with
          | Empty -> None
          | Node (l, (v, cpt), r) ->
              let posv = l.size + cpt in
              if posv >= med && med >= l.size then Some v
              else if posv < med then aux (med - l.size - cpt) r
              else aux med l
        in
        aux med t
end
