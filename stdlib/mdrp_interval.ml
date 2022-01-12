module Make (V : Mdrp_value.Value) = struct
  type t = V.t * V.t

  exception Non_empty_intersection of t * t

  let create i1 i2 =
    assert (i1 <= i2);
    (i1, i2)

  let compare ((l1, u1) as t1) ((l2, u2) as t2) =
    if l1 = l2 && u1 = u2 then 0
    else if u1 <= l2 then -1
    else if u2 <= l1 then 1
    else raise (Non_empty_intersection (t1, t2))

  let included v (lb, ub) = v >= lb && v < ub

  let pp ppf (lb, ub) = Format.fprintf ppf "[%a, %a[" V.pp lb V.pp ub

  let lower = fst

  let upper = snd

  module Map = struct
    type key = t

    type +'a t = (key * 'a) list

    let empty = []

    let add_exn k1 v1 t =
      let rec aux acc l =
        match l with
        | [] -> List.rev_append acc [ (k1, v1) ]
        | ((k2, _) as i2) :: tl ->
            let c = compare k2 k1 in
            if c < 0 then aux (i2 :: acc) tl
            else if c = 0 then List.rev_append acc l
            else List.rev_append acc ((k1, v1) :: l)
      in
      aux empty t

    let find_opt vt m =
      let rec aux = function
        | [ (_, v) ] -> Some v
        | (k, v) :: _ when included vt k -> Some v
        | _ :: tl -> aux tl
        | [] -> None
      in
      aux m

    let min_opt = function (k, _) :: _ -> Some (lower k) | [] -> None

    let rec max_opt = function
      | [ (k, _) ] -> Some (upper k)
      | [] -> None
      | _ :: tl -> max_opt tl

    let pp v_pp ppf (t : 'a t) =
      Format.(
        pp_print_list ~pp_sep:pp_print_cut (fun ppf (k, v) ->
            Format.fprintf ppf "%a => %a" pp k v_pp v))
        ppf t
  end
end
