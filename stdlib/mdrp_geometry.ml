(* http://www.faqs.org/faqs/graphics/algorithms-faq/ *)

module Point = struct
  type t = { x : int; y : int }

  let of_coords x y = { x; y } [@@inlined]

  let distance t1 t2 =
    (* (\* Manhattan distance, we don't need to do complicated things *\)
     * abs (t1.x - t2.x) + abs (t1.y - t2.y) *)
    let dx = float t1.x -. float t2.x in
    let dy = float t1.y -. float t2.y in
    sqrt ((dx *. dx) +. (dy *. dy))

  let manhattan_distance t1 t2 =
    (* Manhattan distance, we don't need to do complicated things *)
    abs (t1.x - t2.x) + abs (t1.y - t2.y)

  let pp ppf { x; y } = Format.fprintf ppf "{x: %d; y: %d}" x y
end

module Segment = struct
  type t = { p1 : Point.t; p2 : Point.t }

  let pp ppf { p1; p2 } = Format.fprintf ppf "[%a; %a]" Point.pp p1 Point.pp p2
  let x_coord { p1; p2 } = p2.Point.x - p1.Point.x [@@inlined]
  let y_coord { p1; p2 } = p2.Point.y - p1.Point.y [@@inlined]
  let of_points p1 p2 = { p1; p2 } [@@inlined]

  let line_point { p1; p2 } n d =
    Point.of_coords
      (p1.x + (n * (p2.x - p1.x) / d))
      (p1.y + (n * (p2.y - p1.y) / d))
    [@@inlined]

  let inverse { p1; p2 } = { p1 = p2; p2 = p1 } [@@inlined]

  (** checks that 0 <= t/d <= 1 without computing t/d *)
  let div_is_gt_zero_and_lower t d =
    (t < 0 && (d > 0 || t < d)) || (t > 0 && (d < 0 || t > d))
    [@@inlined]

  let intersection ab cd =
    (* segment [AB] with A = (xa, ya), B = (xb, yb)
     *
     * a point X is in [AB] if
     * X = A + t (B - A) with t in [0; 1]
     *
     * segment [CD] with C = (xc, yc), D = (xd, yd)
     *
     * a point X is in [CD] if
     * X = C + u (D - C) with u in [0; 1]
     *
     * We want to check that there exists an X = (x, y) in [AB] and [CD] meaning that
     * x = Ax + t (Bx - Ax) = Cx + u * (Dx - Cx)
     * y = Ay + t (By - Ay) = Cy + u * (Dy - Cy)
     * let d = ((Bx - Ax) * (Dy - Cy) - (By - Ay) * (Dx - Cx))
     *   if d = 0 then the two segments are collinear
     * else
     *   let t = ((Ay - Cy) * (Dx - Cx) - (Ax - Cx) * (Dy - Cy)) / d
     *   and u = ((Ay - Cy) * (Bx - Ax) - (Ax - Cx) * (Bx - Ax)) / d
     *   x is in [AB] and [CD] if 0 <= t <= 1 && 0 <= u <= 1
     *   and we can get rid of the division since just want to know if there's an intersection so
     *   0 <= |t| <= |d| && 0 <= |u| <= |d|
     *)
    let d = (x_coord ab * y_coord cd) - (y_coord ab * x_coord cd) in
    if d = 0 then None
    else
      let ca = of_points cd.p1 ab.p1 in
      let t = (y_coord ca * x_coord cd) - (x_coord ca * y_coord cd) in
      (* No intersections if t and d don't have the same sign or |t| > |d| *)
      if div_is_gt_zero_and_lower t d then None
      else
        let u = (y_coord ca * x_coord ab) - (x_coord ca * y_coord ab) in
        (* No intersections if u and d don't have the same sign or |u| > |d| *)
        if div_is_gt_zero_and_lower u d then None
        else
          Some
            ( float ab.p1.x +. (float t *. float (x_coord ab) /. float d),
              float ab.p1.y +. (float t *. float (y_coord ab) /. float d) )
end
