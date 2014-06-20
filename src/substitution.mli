module Env : Map.S with type key = string
type subst = Term.t Env.t

type sym = {name:string ; args:string list; id:int}
type call = sym * Term.t Env.t

val mk_sym: ?args:string list -> string -> sym

val pattern: subst -> Term.t -> Ast.pattern -> subst
val expr: subst -> Ast.expr -> Term.t * call list

val compare : subst -> subst -> int
val equal : subst -> subst -> bool 
val union : subst -> subst -> subst

val compose: subst -> subst -> subst

val defun: Ast.defun list -> call list list
