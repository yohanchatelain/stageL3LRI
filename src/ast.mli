
(* Syntaxe abstraite pour SCT *)

type pattern =
  | Pvar of string
  | Pconstr of string * pattern list
  | Pwild

type expr =
  | Evar of string
  | Econstr of string * expr list
  | Ecall of string * expr list
  | Elet of string * expr * expr
  | Eif of expr * expr * expr
  | Ematch of expr * branch list

and branch =
  pattern * expr

type decl =
  | Dconstructor of string * int
  | Dfunction of (string * string list * expr) list

type program =
  decl list



