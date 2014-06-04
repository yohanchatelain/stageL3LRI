
module Env : Map.S with type key = string
type subst = Term.t Env.t

type sym = {name:string ; args:string list; id:int}
type call = sym * Term.t Env.t

val pattern: subst -> Term.t -> Ast.pattern -> subst
val expr: subst -> Ast.expr -> Term.t * call list

val defun: Ast.defun list -> call list list
