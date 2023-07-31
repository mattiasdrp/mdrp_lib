module Make (V : sig
  include Mdrp_types.Printable
  include Mdrp_types.Comparable with type t := t
end) : sig
  include Mdrp_types.Printable
  include Mdrp_types.Comparable with type t := t

  (** The type of an interval.
      - Lower bound is inclusive
      - Upper bound is exclusive *)

  val pp : Format.formatter -> t -> unit
  val create : V.t -> V.t -> t
  val included : V.t -> t -> bool

  (* Distinct Interval Map. The intervals can't intersect. *)
  module Set : sig
    type key = t
    type 'a t

    val empty : 'a t
    val add_exn : V.t * V.t -> (V.t * V.t) t -> (V.t * V.t) t
    val mem : 'a -> ('a * 'a) t -> bool
    val min_opt : ('a * 'b) t -> 'a option
    val max_opt : ('a * 'b) t -> 'b option
    val pp : Format.formatter -> (V.t * V.t) t -> unit
  end

  (* Distinct Interval Map. The intervals can't intersect. *)
  module Map : sig
    type key = t
    type +'a t

    val empty : 'a t

    val add_exn : key -> 'a -> 'a t -> 'a t
    (** Adds a (key, value) binding in the map.
        Raises an exception if they interval used as the key intersects an already existing interval in the map. *)

    val find_opt : V.t -> 'a t -> 'a option
    val min_opt : 'a t -> V.t option
    val max_opt : 'a t -> V.t option

    val pp :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end
end
