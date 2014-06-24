type t = private
  | Tvar of string
  | Tcons of string * t
  | Tuple of t list
  | Tdestruc of string * t
  | Tproject of int * t
  | Tsum of t list
  | Tapprox of Infinity.t * t
  | Terror

(* smart constructors *)

val var: string -> t
val cons: string -> t -> t
val destruct: string -> t -> t
val tuple: t list -> t
val project: int -> t -> t
val sum: t list -> t
val approx: Infinity.t -> t -> t
val error: unit -> 'a

val is_sum : t -> bool
val list_of_sum : t -> t list

val nb_destr : t -> int
val nf: t -> t

val subst: string -> t -> t -> t

val decreasing_parameter : t -> t list
val test : t -> t*t  -> bool

val less : t*t -> bool
val check_coherent: t*t -> bool
