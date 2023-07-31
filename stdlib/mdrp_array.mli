include module type of Stdlib.Array

val pp :
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  ?left:string ->
  ?right:string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a array ->
  unit
(** Provided a way to print ['a], pretty prints an ['a t].
    By default,
      - [sep] is [;],
      - [left] is [\[|],
      - [right] is [|\]] *)

val compare : ('a -> 'a -> int) -> 'a array -> 'a array -> int
(** Provided a way to compare ['a], compare two ['a array] *)

val equal : ('a -> 'a -> bool) -> 'a array -> 'a array -> bool
(** Provided a way to test the equality of ['a], compare two ['a array] *)

val swap : 'a array -> int -> int -> unit
(** [swap a i j] modifies array [a] in place, swapping [a.(i)] and [a.(j)] *)

val fold_lefti : (int -> 'acc -> 'a -> 'acc) -> 'acc -> 'a array -> 'acc
(** Same as {!fold_left}, but the function is applied to the index of the element
    as first argument, and the element itself as second argument. *)

val get_and_remove : 'a array -> int -> 'a
(** [get_and_remove a i] returns [a.(i)] and removes it from [a] by shifting
    all the elements if index [j] such that [j > i] to the right *)

val out_of_bounds : 'a array -> int -> bool
(** Helper function to avoid having to write [i < 0 || i >= Array.length a] *)

module Matrix : sig
  type 'a t = 'a array array
  (** All functions returning [int * int] return [row * col] *)

  val out_of_bounds : 'a array array -> row:int -> col:int -> bool
  (** Helper function to avoid having to write
      [col < 0 || row < 0 || col >= Array.length t.(0) || row >= Array.length t] *)

  val moore_neighbourhood :
    ?keep_centre:bool ->
    'a array array ->
    row:int ->
    col:int ->
    (int * int) Seq.t
  (** Sequence of all the cells in the {{:https://en.wikipedia.org/wiki/Moore_neighborhood}Moore neighbourhood} of a cell.
      This corresponds to the eight cells around a cell and,
      if [keep_centre = true], the cell itself *)

  val neumann_neighbourhood :
    ?keep_centre:bool ->
    'a array array ->
    row:int ->
    col:int ->
    (int * int) Seq.t
  (** Sequence of all the cells in the {{:https://en.wikipedia.org/wiki/Von_Neumann_neighborhood}Von Neumann neighbourhood} of a cell.
      This corresponds to the four adjacent cells to a cell and,
      if [keep_centre = true], the cell itself *)

  val queen_move :
    ('a array array -> row:int -> col:int -> bool) ->
    'a array array ->
    row:int ->
    col:int ->
    (int * int) Seq.t
  (** [queen_move pred t ~row ~col] is the sequence of the first cell [(row, col)]
      in all directions such that [pred m ~row ~col] is [true].

      As an example [queen_move (fun _ ~row:_ ~col:_ -> true) is the same as {!moore_neighborhood}. *)

  val pp :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    ?left:string ->
    ?right:string ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a array array ->
    unit

  val pp_short :
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a array array ->
    unit

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a array array -> 'acc
  (** [fold_left f init m] computes [f (... (f (f init m.(0).(0)) m.(0).(1)) ...) m.(r-1).(c-1)], where r is the number of rows and c the number of columns n is the length of of the matrix m . *)

  val fold_lefti :
    ('acc -> row:int -> col:int -> 'a -> 'acc) -> 'acc -> 'a array array -> 'acc
  (** [fold_lefti f init m] computes [f (... (f (f init ~row:0 ~col:0 m.(0).(0)) ~row:0 ~col:1 m.(0).(1)) ...) ~row:(r-1) ~col:(c-1) m.(r-1).(c-1)], where r is the number of rows and c the number of columns n is the length of of the matrix m . *)

  val iter : ('a -> unit) -> 'a array array -> unit
  (** [iter f m] computes [f m.(0).(0)); f m.(0).(1) ...; f m.(r-1).(c-1)], where r is the number of rows and c the number of columns n is the length of of the matrix m . *)

  val iteri : (row:int -> col:int -> 'a -> unit) -> 'a array array -> unit
  (** [iteri f m] computes [f ~row:0 ~col:0 m.(0).(0)); f ~row:0 ~col:1 m.(0).(1) ...; f ~row:(r-1) ~col:(c-1) m.(r-1).(c-1)], where r is the number of rows and c the number of columns n is the length of of the matrix m . *)

  val rows_columns : 'a array array -> int * int
  (** [rows_columns m] returns the number of rows and columns of the matrix [m] *)
end
