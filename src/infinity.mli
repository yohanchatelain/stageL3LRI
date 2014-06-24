type t =
 | Infinity
 | Int of int

val add: t -> t -> t
val add_int : int -> t -> t
val compare: t -> t -> int
val equal : t -> t -> bool
val max : t -> t -> t
val print : t -> unit
