include module type of Stdlib.Seq

val next_opt : 'a t -> 'a option * 'a t
(** Returns [Some (next element)] in a sequence if it exists or [None] and the next sequence *)

val all_head_tails : 'a list -> ('a * 'a list) t
(** Provided a list, returns a new sequence of all the ways this list can be splitted
 * with one element and the rest of the list
 * Example:
 * all_head_tails [1; 2; 3; 4] will return
 * 1, [2; 3; 4]
 * 2, [1; 3; 4]
 * 3, [1; 2; 4]
 * 4, [1; 2; 3]
 * *)

val permutations : 'a list -> 'a list t
(** Returns a sequence that computes all the permutations in a list *)

val all_splits : 'a list -> ('a list * 'a list) t
(** Returns a sequence that computes all the splits of a list
 * Example:
 * all_splits [1; 2; 3] will return
 * [1], [2; 3]
 * [1; 2] [3]
 * *)

val find_opt : ('a -> bool) -> 'a t -> 'a option

val findi_opt : (int -> 'a -> bool) -> 'a t -> 'a option

val find_map : ('a -> 'b option) -> 'a t -> 'b option

val findi_map : (int -> 'a -> 'b option) -> 'a t -> 'b option

val find_fold : ('b -> 'a -> 'b * bool) -> 'b -> 'a t -> ('a * 'b) option
