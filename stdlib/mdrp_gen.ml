let init = Random.init

let char () = char_of_int (33 + Random.int 94)

let int n = Random.int n

let fixed_string n = String.init n (fun _ -> char ())

let string () = fixed_string (1 + Random.int 10)

module type DigitValue = sig
  include Mdrp_value.Value

  val zero : t

  val random : t -> t
end

module Distributed (V : DigitValue) = struct
  module VM = Mdrp_interval.Make (V)
  module VMap = VM.Map

  type 'a t = { mutable distributions : 'a VMap.t; mutable max : V.t }

  let init () = { distributions = VMap.empty; max = V.zero }

  let add lb ub v t =
    t.distributions <- VMap.add_exn (VM.create lb ub) v t.distributions;
    t.max <-
      (match VMap.max_opt t.distributions with Some v -> v | None -> V.zero)

  let pp v_pp ppf t =
    Format.fprintf ppf "%a@,%a" V.pp t.max (VMap.pp v_pp) t.distributions

  let get t =
    let r = V.random t.max in
    match VMap.find_opt r t.distributions with
    | Some v -> v
    | None -> assert false
end
