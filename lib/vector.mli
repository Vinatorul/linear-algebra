type orientation = Row | Column

type t = {
  data : Matrix.t;
  orientation : orientation;
}

val create : orientation -> int -> float -> t

val length : t -> int

val get : t -> int -> float

val set : t -> int -> float -> unit

val copy : t -> t

val of_array : orientation -> float array -> t

val to_array : t -> float array

val map : (float -> float) -> t -> t

val mapi : (int -> float -> float) -> t -> t

val fold : ('a -> float -> 'a) -> 'a -> t -> 'a

val equal : t -> t -> bool

val to_matrix : t -> Matrix.t

val of_matrix : Matrix.t -> orientation -> t

val orientation : t -> orientation

val dot : t -> t -> float

val add : t -> t -> t

val sub : t -> t -> t

val scale : float -> t -> t