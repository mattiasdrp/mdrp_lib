type 'a ring = { left : 'a list; right : 'a list }

let empty = { left = []; right = [] }

let add x r = { r with left = x :: r.left }

let add_tl x r = { r with right = x :: r.right }

let remove r =
  match r.right with
  | _ :: right -> { r with right }
  | [] -> (
      match List.rev r.left with
      | [] -> failwith "Empty ring"
      | _ :: right -> { left = []; right })

let next { left; right } =
  match right with
  | [] -> (
      match List.rev left with
      | hd :: right -> (hd, { left = [ hd ]; right })
      | _ -> failwith "Empty ring")
  | hd :: right -> (hd, { left = hd :: left; right })

let prev { left; right } =
  match left with
  | [] -> (
      match List.rev right with
      | hd :: left -> (hd, { left; right = [ hd ] })
      | _ -> failwith "Empty ring")
  | hd :: left -> (hd, { left; right = hd :: right })

let elements { left; right } = List.rev_append left right

let pp pp_e ppf r = Format.fprintf ppf "%a↩" Mdrp_list.(pp pp_e) (elements r)

let pp_ring r =
  let cpt = ref 0 in
  let elts = elements r in
  let rad = 8 in
  let angle = float (List.length elts) /. (2. *. Float.pi) in
  let cos =
    Mdrp_int.Set.of_list
      (List.mapi (fun i _ -> truncate (cos (float i *. angle))) elts)
  in
  for i = -rad to rad do
    for j = -rad to rad do
      if
        Mdrp_int.Decimal.sqrt ((i * i) + (j * j)) = rad
        && Mdrp_int.Set.mem i cos
      then (
        incr cpt;
        Format.eprintf "* ")
      else Format.eprintf "  "
    done;
    Format.eprintf "@."
  done;
  Format.eprintf "%d@." !cpt
