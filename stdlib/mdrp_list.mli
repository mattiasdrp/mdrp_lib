include module type of Stdlib.List

val pp :
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  ?left:string ->
  ?right:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a t ->
  unit
(** Provided a way to print ['a], pretty prints an ['a t].
    By default,
      - [sep] is [;],
      - [left] is [\[],
      - [right] is [\]] *)

val combinations : add:('a -> 'a -> 'a) -> zero:'a -> 'a t -> 'a -> 'a t t
(** Provided a sum function and a zero element, [combinations ~add ~zero l n]
    returns all the combinations of elements in [l] that, summed, equal to [n].
    Not tail recursive *)

val permutations : 'a t -> 'a t t
(** Returns all the permutations in a list
    Not tail recursive *)

val rotations : 'a t -> 'a t t
(** Returns all the rotations of a list
    Not tail recursive *)

val take : int -> 'a t -> 'a t
(** [take n l] returns the [n] first elements of [l]
    Not tail recursive *)

val cartesian_product : 'a t -> 'b t -> ('a * 'b) t
(** Returns the cartesian product of two lists
    Tail recursive *)

val fold_lefti : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
