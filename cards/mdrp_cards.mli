exception NotInBound of int * (int * int)

module Value : sig
  type t = private King | Queen | Jake | Number of int

  val of_char : char -> t

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val follows : t -> t -> bool
  (** [follows v1 v2] checks if [v1] is following immediately [v2]
      - [follows King Queen] returns true
      - [follows Number 2 Queen] returns false
      *)
end

module Suit : sig
  type t = Club | Diamond | Heart | Spade

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val compare : t -> t -> int
end

type t = { value : Value.t; suit : Suit.t }

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool

val compare : t -> t -> int

val of_int : int -> t

val of_string : string -> t
