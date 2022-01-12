open Mdrp_lib
module Card = Mdrp_cards

module Hand = struct
  open Card

  type t =
    | High_card of Value.t list
    | One_pair of { pair : Value.t; rest : Value.t list }
    | Two_pairs of { hpair : Value.t; lpair : Value.t; rest : Value.t }
    | Three_of_a_kind of { three : Value.t; rest : Value.t list }
    | Straight of { hcard : Value.t; flushed : bool }
    | Flush of Value.t list
    | Full_house of { three : Value.t; pair : Value.t }
    | Four_of_a_kind of { four : Value.t; rest : Value.t }

  let pp_list ppf l = List.(pp Value.pp ppf) l

  let vpp = Value.pp

  let pp ppf = function
    | High_card l -> Format.fprintf ppf "High : %a" pp_list l
    | One_pair { pair; rest } ->
        Format.fprintf ppf "Pair : 2x%a, %a" vpp pair pp_list rest
    | Two_pairs { hpair; lpair; rest } ->
        Format.fprintf ppf "Two pairs : 2x%a, 2x%a, %a" vpp hpair vpp lpair vpp
          rest
    | Three_of_a_kind { three; rest } ->
        Format.fprintf ppf "Three : 3x%a, %a" vpp three pp_list rest
    | Straight { hcard; flushed } ->
        Format.fprintf ppf "Straight%s : %a"
          (if flushed then " Flush" else "")
          vpp hcard
    | Flush l -> Format.fprintf ppf "Flush %a" pp_list l
    | Full_house { three; pair } ->
        Format.fprintf ppf "Full house: 3x%a, 2x%a" vpp three vpp pair
    | Four_of_a_kind { four; rest } ->
        Format.fprintf ppf "Four: 4x%a, %a" vpp four vpp rest

  let compare h1 h2 =
    match (h1, h2) with
    | High_card cl1, High_card cl2 -> List.compare Value.compare cl1 cl2
    | High_card _, _ -> -1
    | _, High_card _ -> 1
    | One_pair h1, One_pair h2 ->
        let cc = Value.compare h1.pair h2.pair in
        if cc = 0 then List.compare Value.compare h1.rest h2.rest else cc
    | One_pair _, _ -> -1
    | _, One_pair _ -> 1
    | Two_pairs h1, Two_pairs h2 ->
        let cc = Value.compare h1.hpair h2.hpair in
        if cc = 0 then
          let cc = Value.compare h1.lpair h2.lpair in
          if cc = 0 then Value.compare h1.rest h2.rest else cc
        else cc
    | Two_pairs _, _ -> -1
    | _, Two_pairs _ -> 1
    | Three_of_a_kind h1, Three_of_a_kind h2 ->
        let cc = Value.compare h1.three h2.three in
        if cc = 0 then List.compare Value.compare h1.rest h2.rest else cc
    | Three_of_a_kind _, _ -> -1
    | _, Three_of_a_kind _ -> 1
    | Straight h1, Straight h2 ->
        if h1.flushed && not h2.flushed then 1
        else if (not h1.flushed) && h2.flushed then -1
        else Value.compare h1.hcard h2.hcard
    | Straight _, _ -> -1
    | _, Straight _ -> 1
    | Flush cl1, Flush cl2 -> List.compare Value.compare cl1 cl2
    | Flush _, _ -> -1
    | _, Flush _ -> 1
    | Full_house h1, Full_house h2 ->
        let cc = Value.compare h1.three h2.three in
        if cc = 0 then Value.compare h1.pair h2.pair else cc
    | Full_house _, _ -> -1
    | _, Full_house _ -> 1
    | Four_of_a_kind h1, Four_of_a_kind h2 ->
        let cc = Value.compare h1.four h2.four in
        if cc = 0 then Value.compare h1.rest h2.rest else cc

  let straight l =
    match l with
    | Value.[ Number 1; Number 5; Number 4; Number 3; Number 2 ] ->
        Some Value.(of_char '4')
    | hd :: tl ->
        if
          snd
            (List.fold_left
               (fun (prev, res) v -> (v, res && Value.follows prev v))
               (hd, true) tl)
        then Some hd
        else None
    | _ -> assert false

  let evaluate l =
    let open Card in
    let module V = Mdrp_lib.Multiset.Make (Mdrp_cards.Value) in
    let rec aux suit values = function
      | [] ->
          ( suit,
            V.elements values
            |> List.fast_sort (fun (v1, m1) (v2, m2) ->
                   let ci = Int.compare m2 m1 in
                   if ci = 0 then Value.compare v2 v1 else ci) )
      | c :: tl ->
          let suit =
            Option.bind suit (fun suit ->
                if Suit.equal c.suit suit then Some suit else None)
          in
          aux suit (V.add c.value values) tl
    in
    let suit, l =
      match l with
      | c1 :: tl -> aux (Some c1.suit) (V.singleton c1.value) tl
      | _ -> assert false
    in
    match l with
    | [ (four, 4); (rest, 1) ] -> Four_of_a_kind { four; rest }
    | [ (three, 3); (pair, 2) ] -> Full_house { three; pair }
    | [ (three, 3); (r1, 1); (r2, 1) ] ->
        Three_of_a_kind { three; rest = [ r1; r2 ] }
    | [ (hpair, 2); (lpair, 2); (rest, 1) ] -> Two_pairs { hpair; lpair; rest }
    | [ (pair, 2); (r1, 1); (r2, 1); (r3, 1) ] ->
        One_pair { pair; rest = [ r1; r2; r3 ] }
    | [ (_, 1); (_, 1); (_, 1); (_, 1); (_, 1) ] as l -> (
        let l = List.map (fun (v, _) -> v) l in
        match (suit <> None, straight l) with
        | flushed, Some c -> Straight { hcard = c; flushed }
        | true, _ -> Flush l
        | false, _ -> High_card l
        (* | Straight of { hcard : t; flushed : bool } *))
    | _ -> assert false
end
