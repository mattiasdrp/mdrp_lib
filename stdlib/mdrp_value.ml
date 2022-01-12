module type Value = sig
  type t

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int
end
