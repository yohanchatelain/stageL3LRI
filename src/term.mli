
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
val destruct: string -> t list -> t

(* ... *)

