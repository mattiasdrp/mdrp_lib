include module type of Stdlib.Array

val pp :
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  ?left:string ->
  ?right:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a array ->
  unit
(** Provided a way to print ['a], pretty prints an ['a array].
    By default,
      - [sep] is [;],
      - [left] is [\[|],
      - [right] is [|\]] *)

val swap : 'a array -> int -> int -> unit
(** [swap a i j] Swaps the values in [a] at indices [i] and [j] *)

val fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a
(** [fold_lefti f init a] computes
   [f (n-1) (... (f 1 (f 0 init a.(0)) a.(1)) ...) a.(n-1)],
   where [n] is the length of the array [a]. *)

val get_and_remove : 'a array -> int -> 'a

val ( .?() ) : 'a array -> int -> 'a option

val pp_matrix :
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  ?left:string ->
  ?right:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a array array ->
  unit
(** Provided a way to print ['a], pretty prints an ['a array array].
    By default,
      - [sep] is [;],
      - [left] is [\[|],
      - [right] is [|\]] *)
