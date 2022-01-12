include module type of Stdlib.List

val pp :
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  ?left:string ->
  ?right:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a list ->
  unit
(** Provided a way to print ['a], pretty prints an ['a list].
    By default,
      - [sep] is [;],
      - [left] is [\[],
      - [right] is [\]] *)

val combinations :
  add:('a -> 'a -> 'a) -> zero:'a -> 'a list -> 'a -> 'a list list
(** Provided a sum function and a zero element, [combinations ~add ~zero l n]
    returns all the combinations of elements in [l] that, summed, equal to [n].
    Not tail recursive *)

val permutations : 'a list -> 'a list list
(** Returns all the permutations in a list
    Not tail recursive *)

val rotations : 'a list -> 'a list list
(** Returns all the rotations of a list
    Not tail recursive *)

val fold_lefti : ('a -> int -> 'b -> 'a) -> 'a -> 'b list -> 'a
