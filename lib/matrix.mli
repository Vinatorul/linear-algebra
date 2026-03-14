type t = float array array

val create : int -> int -> float -> t

val rows : t -> int

val cols : t -> int

val get : t -> int -> int -> float

val set : t -> int -> int -> float -> unit

val copy : t -> t

val of_array : float array array -> t

val to_array : t -> float array array

val map : (float -> float) -> t -> t

val mapi : (int -> int -> float -> float) -> t -> t

val fold : ('a -> float -> 'a) -> 'a -> t -> 'a

val equal : t -> t -> bool
