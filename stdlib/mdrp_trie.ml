module type CharS = sig
  type t

  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

module type WordS = sig
  module C : CharS

  type t

  val empty : t
  val to_list : t -> C.t list
  val pp : Format.formatter -> t -> unit
  val prepend : C.t -> t -> t
  val append : t -> C.t -> t
end

module type TrieS = sig
  type key
  type elt
  type t

  module CSet : Mdrp_set.S with type elt := key

  module IMap : sig
    include module type of Mdrp_int.Map
  end

  val empty : t
  val add : elt -> t -> t
  val remove : elt -> t -> t

  val bits :
    ?here:key Mdrp_int.Map.t ->
    ?absent:CSet.t ->
    ?other:CSet.t IMap.t ->
    t ->
    int

  val pp : Format.formatter -> t -> unit
  val fold : (t -> elt -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make (W : WordS) : TrieS with type elt = W.t and type key = W.C.t =
struct
  type key = W.C.t

  module M = Mdrp_map.Make (W.C)

  type elt = W.t
  type t = { word : bool; card : int; branches : t M.t }

  let rec pp ppf { branches; card; _ } =
    Format.fprintf ppf "@[<v 1>(%d)@,%a@]" card (M.pp pp) branches

  let empty = { word = false; card = 0; branches = M.empty }
  let is_empty t = (not t.word) && M.is_empty t.branches

  let fold f t acc =
    let rec aux word t acc =
      let acc = if t.word then f t word acc else acc in
      M.fold (fun k t acc -> aux W.(append word k) t acc) t.branches acc
    in
    aux W.empty t acc

  let add w t =
    let rec aux_add t = function
      | [] -> if t.word then t else { t with card = 1; word = true }
      | c :: cl ->
          let b = try M.find c t.branches with Not_found -> empty in
          let new_b = aux_add b cl in
          {
            t with
            card = t.card - b.card + new_b.card;
            branches = M.add c new_b t.branches;
          }
    in
    aux_add t (W.to_list w)

  let remove w t =
    let rec aux_rem t = function
      | [] -> { t with card = t.card - 1; word = false }
      | c :: cl -> (
          match M.find c t.branches with
          | s ->
              let s = aux_rem s cl in
              let new_branches =
                if is_empty s then M.remove c t.branches
                else M.add c s t.branches
              in
              { t with card = t.card - 1; branches = new_branches }
          | exception Not_found -> t)
    in
    aux_rem t (W.to_list w)

  module CSet = Mdrp_set.Make (W.C)
  module IMap = Mdrp_int.Map

  let other_exists c d m =
    match IMap.find d m with
    | cs -> CSet.exists (( = ) c) cs
    | exception Not_found -> false

  (* here is a map depth -> C.t stating that c is the only C.t available at depth *)
  (* absent is a set of all C.t that can not exist in the word *)
  (* other is a map depth -> C.t stating that c exists but not at this depth *)
  let bits ?(here = IMap.empty) ?(absent = CSet.empty) ?(other = IMap.empty) t =
    let rec aux d t acc =
      match IMap.find d here with
      | c ->
          M.fold
            (fun k t acc -> if k = c then aux (d + 1) t acc else acc - t.card)
            t.branches acc
      | exception Not_found ->
          (* if d -> k is in other or if k is in absent, don't explore this branch *)
          (* otherwise, fold on it *)
          M.fold
            (fun k t acc ->
              if CSet.mem k absent || other_exists k d other then acc - t.card
              else aux (d + 1) t acc)
            t.branches acc
    in
    aux 1 t t.card
end
