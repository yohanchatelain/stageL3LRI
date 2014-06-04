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

