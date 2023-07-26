module Make (V : sig
  include Mdrp_types.Printable
  include Mdrp_types.Comparable with type t := t
end) =
struct
  type t = V.t * V.t

  exception Non_empty_intersection of t * t

  let create i1 i2 =
    if V.compare i1 i2 > 0 then
      failwith
        (Format.asprintf "[%a, %a] is an empty interval@." V.pp i1 V.pp i2);
    (i1, i2)

  let compare ((l1, u1) as t1) ((l2, u2) as t2) =
    if V.compare l1 l2 = 0 && V.compare u1 u2 = 0 then 0
    else if V.compare u1 l2 <= 0 then -1
    else if V.compare u2 l1 <= 0 then 1
    else raise (Non_empty_intersection (t1, t2))

  let included v (lb, ub) = v >= lb && v < ub
  let pp ppf (lb, ub) = Format.fprintf ppf "[%a, %a[" V.pp lb V.pp ub
  let lower = fst
  let upper = snd

  module Set = struct
    type key = t
    type +'a t = 'a list

    let empty = []

    let add_exn k1 t =
      let rec aux acc l =
        match l with
        | [] -> List.rev_append acc [ k1 ]
        | k2 :: tl ->
            let c = compare k2 k1 in
            if c < 0 then aux (k2 :: acc) tl
            else if c = 0 then List.rev_append acc l
            else List.rev_append acc (k1 :: l)
      in
      aux empty t

    let mem k t =
      let rec aux = function
        | [] -> false
        | k' :: _ when included k k' -> true
        | _ :: tl -> aux tl
      in
      aux t

    let min_opt = function k :: _ -> Some (lower k) | [] -> None

    let rec max_opt = function
      | [] -> None
      | [ k ] -> Some (upper k)
      | _ :: tl -> max_opt tl

    let pp ppf (t : 'a t) =
      Format.(
        pp_print_list ~pp_sep:pp_print_cut (fun ppf k ->
            Format.fprintf ppf "%a" pp k))
        ppf t
  end

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

(* I may work one day on this but no point right now *)

(* module Interval = struct *)
(*   type t = (int * int) list *)

(*   let add i (t : t) = *)
(*     let rec aux ((low, up) as itvl) acc l = *)
(*       match l with *)
(*       (\* We reached the end, append *\) *)
(*       | [] -> List.rev_append acc [ (low, up) ] *)
(*       (\* low ------ up *)
(*                         low' -------- up' *\) *)
(*       | (low', _) :: _ as l when up < low' - 1 -> List.rev_append acc (itvl :: l) *)
(*       (\* low ---------- up *)
(*                low' -------- up' *)

(*          or *)

(*          low ---------------------------- up *)
(*                low' -------- up' *)

(*          or *)

(*           _       low ---------- up *)
(*          low' -------- up'*\) *)
(*       | (low', up') :: tl when low <= up' && up >= low' - 1 -> *)
(*           aux (min low low', max up up') acc tl *)
(*       (\* _       low ---------- up *)
(*          low' -------- up' *\) *)
(*       | i :: tl -> aux itvl (i :: acc) tl *)
(*     in *)

(*     aux i [] t *)

(*   let remove i (t : t) = *)
(*     let rec aux ((low, up) as itvl) acc l = *)
(*       Format.eprintf "removing (%d, %d) from %a@." low up *)
(*         List.(pp Pair.(pp Int.pp Int.pp)) *)
(*         l; *)
(*       match l with *)
(*       (\* We reached the end, nothing to remove *\) *)
(*       | [] -> List.rev acc *)
(*       (\* low ------ up *)
(*                         low' -------- up' *\) *)
(*       | (low', _) :: _ as l when up < low' -> List.rev_append acc l *)
(*       (\* low ---------------------------- up *)
(*                low' -------- up' *\) *)
(*       | (low', up') :: tl when low <= low' && up >= up' -> *)
(*           List.rev_append acc tl *)
(*       (\* low ---------- up *)
(*                low' -------- up' *\) *)
(*       | (low', up') :: tl when low <= low' && up >= low' && up < up' -> *)
(*           List.rev_append acc ((up + 1, up') :: tl) *)
(*       (\* _       low ---------- up *)
(*          low' -------- up' *\) *)
(*       | (low', up') :: tl when low > low' && low <= up' && up >= up' -> *)
(*           aux (up', up) acc ((low', low - 1) :: tl) *)
(*       (\* _     low --------- up *)
(*          low' ------------------- up' *\) *)
(*       | (low', up') :: tl when low > low' && up < up' -> *)
(*           List.rev_append acc ((low', low) :: (up, up') :: tl) *)
(*       (\* Nothing to do here, move forward *\) *)
(*       | i :: tl -> aux itvl (i :: acc) tl *)
(*     in *)
(*     aux i [] t *)

(*   let toggle i (t : t) = *)
(*     let rec aux ((low, up) as itvl) acc l = *)
(*       Format.eprintf "toggling (%d, %d) from %a@." low up *)
(*         List.(pp Pair.(pp Int.pp Int.pp)) *)
(*         l; *)
(*       match l with *)
(*       (\* We reached the end, nothing to remove, toggle on the itvl *\) *)
(*       | [] -> List.rev_append acc [ itvl ] *)
(*       (\* The current interval is too high, nothing to remove in it, toggle on the itvl *\) *)
(*       | (low', _) :: _ as l when up < low' -> List.rev_append acc (itvl :: l) *)
(*       (\* low ---------- up *)
(*                low' -------- up' *\) *)
(*       | (low', up') :: tl when low < low' && up > low' && up < up' -> *)
(*           List.rev_append acc ((low, low' - 1) :: (up + 1, up') :: tl) *)
(*       (\* _     low --------- up *)
(*          low' ------------------- up' *\) *)
(*       | (low', up') :: tl when low' < low && up' > up -> *)
(*           List.rev_append acc ((low', low - 1) :: (up + 1, up') :: tl) *)
(*       (\* _       low ---------- up *)
(*          low' -------- up' *\) *)
(*       | (low', up') :: tl when low > low' && low < up' && up > up' -> *)
(*           aux (up' + 1, up) ((low', low - 1) :: acc) tl *)
(*       (\* low ---------------------------- up *)
(*                low' -------- up' *\) *)
(*       | (low', up') :: tl when low < low' && up > up' -> *)
(*           aux (up' + 1, up) ((low, low' - 1) :: acc) tl *)
(*       (\* low  --------- up *)
(*          low' --------- up' *\) *)
(*       | (low', up') :: tl when low' = low && up' = up -> List.rev_append acc tl *)
(*       (\* Nothing to do here, move forward *\) *)
(*       | i :: tl -> aux itvl (i :: acc) tl *)
(*     in *)
(*     aux i [] t *)

(*   let length l = *)
(*     let rec aux acc = function *)
(*       | [] -> acc *)
(*       | (low, up) :: tl -> aux (acc + up - low) tl *)
(*     in *)
(*     aux 0 l *)

(* end *)
