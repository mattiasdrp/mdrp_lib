module Int = struct
  type t = { num : int; den : int }

  let pp ppf { num; den } = Format.fprintf ppf "%d/%d" num den

  let create num den = { num; den }

  let get_num t = t.num

  let get_den t = t.den

  let equal t1 t2 = float t1.num /. float t1.den = float t2.num /. float t2.den

  let simplify { num; den } =
    let mnum = Mdrp_int.Decimal.prime_divisors num in
    let mden = Mdrp_int.Decimal.prime_divisors den in
    let gcdb = Mdrp_int.Multiset.inter mnum mden in
    let gcd =
      Mdrp_int.(Multiset.fold (fun d occ acc -> acc * Decimal.pow d occ) gcdb 1)
    in
    { num = num / gcd; den = den / gcd }

  let plus i { num; den } = { num = num + (i * den); den }

  let invert { num; den } = { num = den; den = num }

  let decimals t =
    let { num; den } = simplify t in
    let ht = Hashtbl.create 19 in
    let rec aux n acc =
      let n10 = 10 * n in
      let dec = n10 / den in
      let rem = n10 mod den in
      if rem = 0 || Hashtbl.mem ht (n, den) then
        let acc = if rem = 0 then dec :: acc else acc in
        List.rev acc
      else (
        Hashtbl.add ht (n, den) ();
        (* Format.eprintf "%d/%d = %d > %d@." n d dec rem; *)
        aux rem (dec :: acc))
    in
    aux num []

  let ( * ) t1 t2 = { num = t1.num * t2.num; den = t1.den * t2.den }
end

module Z = struct
  type t = { num : Z.t; den : Z.t }

  let pp ppf { num; den } =
    Format.fprintf ppf "%a/%a" Z.pp_print num Z.pp_print den

  let create num den = { num; den }

  let plus i { num; den } = Z.{ num = num + (i * den); den }

  let invert { num; den } = { num = den; den = num }

  let ( * ) t1 t2 = Z.{ num = t1.num * t2.num; den = t1.den * t2.den }
end
