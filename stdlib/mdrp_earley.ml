module Int = Mdrp_int
module Array = Mdrp_array

module type StringSig = sig
  type token
  type t

  val get : t -> int -> token option
  val eq_token : token -> token -> bool
  val compare_token : token -> token -> int
  val equal_token : token -> token -> bool
  val pp_token : Format.formatter -> token -> unit
end

module MakeParser (StringToParse : StringSig) = struct
  module Symbol = struct
    type t = Terminal of StringToParse.token | NonTerminal of string

    let compare t1 t2 =
      match (t1, t2) with
      | Terminal tok1, Terminal tok2 -> StringToParse.compare_token tok1 tok2
      | Terminal _, NonTerminal _ -> -1
      | NonTerminal _, Terminal _ -> 1
      | NonTerminal s1, NonTerminal s2 -> Stdlib.String.compare s1 s2

    let equal t1 t2 =
      match (t1, t2) with
      | Terminal tok1, Terminal tok2 -> StringToParse.equal_token tok1 tok2
      | NonTerminal s1, NonTerminal s2 -> Stdlib.String.equal s1 s2
      | _ -> false

    let pp ppf = function
      | Terminal tok -> Format.fprintf ppf "%a" StringToParse.pp_token tok
      | NonTerminal s -> Format.fprintf ppf "<%s>" s
  end

  module DottedExpression = struct
    type t = { expression : Symbol.t array; position : int }

    let create expression = { expression; position = 0 }

    let compare t1 t2 =
      let c1 = Int.compare t1.position t2.position in
      if c1 = 0 then Array.compare Symbol.compare t1.expression t2.expression
      else c1

    let equal t1 t2 =
      Int.equal t1.position t2.position
      && Array.equal Symbol.equal t1.expression t2.expression

    let next t =
      try Some (Array.get t.expression t.position)
      with Invalid_argument _ -> None

    let completed_expression { expression; position } =
      position = Array.length expression

    (** [incr_position t] returns [t, bool] where the dot has been moved one step to the right.
          If the new position is on the far right, [bool] is [true] else it is [false] *)
    let incr_position t =
      let position = t.position + 1 in
      let t = { t with position } in
      (t, completed_expression t)

    let pp ppf t =
      Array.iteri
        (fun i s ->
          if i = t.position then Format.fprintf ppf "•";
          Format.fprintf ppf "%a" Symbol.pp s)
        t.expression
  end

  module Rule = struct
    type t = { symbol : Symbol.t; expression : DottedExpression.t }

    let create symbol expression =
      let expression = DottedExpression.create expression in
      { symbol; expression }

    let compare t1 t2 =
      let c1 = Symbol.compare t1.symbol t2.symbol in
      if c1 = 0 then DottedExpression.compare t1.expression t2.expression
      else c1

    let pp ppf t =
      Format.fprintf ppf "%a ::= %a" Symbol.pp t.symbol DottedExpression.pp
        t.expression
  end

  module Rules = struct
    include Mdrp_map.Make (Mdrp_string)
  end

  module Item = struct
    type t = { index : int; rule : Rule.t }

    let create index rule = { index; rule }

    let compare t1 t2 =
      let c1 = Int.compare t1.index t2.index in
      if c1 = 0 then Rule.compare t1.rule t2.rule else c1

    let pp ppf { index; rule } =
      Format.fprintf ppf "{%d; %a}" index Rule.pp rule
  end

  module ItemSet = struct
    include Mdrp_set.Make (Item)
  end

  type t = ItemSet.t Int.Map.t

  open StringToParse

  let scan i string itemset new_items t =
    (* get the symbol at index j *)
    match get string (i - 1) with
    | None -> raise Exit
    | Some token ->
        (* get all the items before *)
        let items_before = Int.Map.find (i - 1) t in
        ItemSet.fold
          (fun (Item.{ rule; _ } as item) (itemset, new_items, completed_items) ->
            (* if the item has the rule A -> α · token ꞵ *)
            (* since we just read token, we can add A -> α token · ꞵ to t[j] *)
            match DottedExpression.next rule.Rule.expression with
            | Some (Terminal token') when StringToParse.eq_token token token' ->
                let expression, completed =
                  DottedExpression.incr_position rule.expression
                in
                let item = { item with rule = { rule with expression } } in
                let completed_items =
                  if completed then ItemSet.add item completed_items
                  else completed_items
                in
                let itemset, new_items =
                  if ItemSet.mem item itemset then (itemset, new_items)
                  else (ItemSet.add item itemset, ItemSet.add item new_items)
                in
                (itemset, new_items, completed_items)
            | _ -> (itemset, new_items, completed_items))
          items_before
          (itemset, new_items, ItemSet.empty)

  let predictor i rules itemset new_items =
    (* Find all the items at index j *)
    ItemSet.fold
      (fun { rule; _ } (itemset, new_items) ->
        match DottedExpression.next rule.Rule.expression with
        (* For each item A -> α · B ꞵ, if B non terminal *)
        (* Add all the rules of the form B -> γ to the items *)
        | Some (NonTerminal s as symbol) ->
            let symbol_arrays = Rules.find s rules in
            List.fold_left
              (fun (itemset, new_items) sa ->
                let rule = Rule.create symbol sa in
                let item = Item.create i rule in
                let itemset, new_items =
                  if ItemSet.mem item itemset then (itemset, new_items)
                  else (ItemSet.add item itemset, ItemSet.add item new_items)
                in
                (itemset, new_items))
              (itemset, new_items) symbol_arrays
        | _ -> (itemset, new_items))
      itemset (itemset, new_items)

  let completor t completed_items itemset new_items =
    let rec aux completed_items itemset new_items =
      let completed_items, itemset, new_items =
        ItemSet.fold
          (fun ({ rule = { symbol; _ }; _ } as completed_item)
               (completed_items, itemset, new_items) ->
            let items_before = Int.Map.find completed_item.index t in
            ItemSet.fold
              (fun ({ rule = { expression; _ } as rule; _ } as item_before)
                   (completed_items, itemset, new_items) ->
                let next = DottedExpression.next expression in
                match next with
                | Some next when Symbol.equal symbol next ->
                    let moved_expression, completed =
                      DottedExpression.incr_position expression
                    in
                    let item_forward =
                      {
                        item_before with
                        rule = { rule with expression = moved_expression };
                      }
                    in
                    let completed_items =
                      if completed then ItemSet.add item_forward completed_items
                      else completed_items
                    in
                    let itemset, new_items =
                      if ItemSet.mem item_forward itemset then
                        (itemset, new_items)
                      else
                        ( ItemSet.add item_forward itemset,
                          ItemSet.add item_forward new_items )
                    in

                    (completed_items, itemset, new_items)
                | _ -> (completed_items, itemset, new_items))
              items_before
              (completed_items, itemset, new_items))
          completed_items
          (ItemSet.empty, itemset, new_items)
      in
      if ItemSet.is_empty completed_items then (itemset, new_items)
      else aux completed_items itemset new_items
    in
    aux completed_items itemset new_items

  exception Res of bool

  let run_ rules axiom string t =
    let rec aux i itemset new_items t =
      match scan i string itemset new_items t with
      | itemset, new_items, completed_items ->
          let itemset, new_items = predictor i rules itemset new_items in

          let itemset, new_items =
            completor t completed_items itemset new_items
          in

          if ItemSet.is_empty new_items then
            (* We finished building the whole t[i], add the itemset to it and
               move forward *)
            aux (i + 1) ItemSet.empty ItemSet.empty (Int.Map.add i itemset t)
          else
            (* New items have been added, let's scan, predict and complete them *)
            aux i itemset ItemSet.empty t
      | exception Exit ->
          (* No more token, we reached the end of the string we wanted to parse
                   Weak. now need to check if the
          *)
          let axiom = Symbol.NonTerminal axiom in
          let items = Int.Map.find (i - 1) t in
          ItemSet.exists
            (fun { rule = { symbol; expression }; _ } ->
              Symbol.equal symbol axiom
              && DottedExpression.completed_expression expression)
            items
    in
    aux 1 ItemSet.empty ItemSet.empty t

  let run rules axiom string =
    let expressions = Rules.find axiom rules in
    let init_itemset =
      List.fold_left
        (fun itemset expression ->
          let item =
            Item.create 0 (Rule.create (NonTerminal axiom) expression)
          in
          ItemSet.add item itemset)
        ItemSet.empty expressions
    in
    let rec aux itemset =
      let itemset, new_items = predictor 0 rules itemset ItemSet.empty in
      if ItemSet.is_empty new_items then Int.Map.singleton 0 itemset
      else aux itemset
    in

    run_ rules axiom string (aux init_itemset)
end
