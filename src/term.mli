
type t = private
  | Tvar of string
  | Tcons of string * t list
  | Tdestruc of string * t
  | Tproject of int * t
  | Tsum of t list
  | Tapprox of infinity * t
  | Terror

val var: string -> t
val cons: string -> t list -> t
(* ... *)
