module type Printable = sig
  type t

  val pp : Format.formatter -> t -> unit
end

module type Comparable = sig
  type t

  val compare : t -> t -> int
end
