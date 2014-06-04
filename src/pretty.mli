open Substitution

val program: Ast.program -> unit
val call: (sym*Term.t Env.t) list -> unit
