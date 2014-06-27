open Substitution

val term: Term.t -> unit
val program: Ast.program -> unit
val call: (sym*Term.t Env.t) list -> unit
val cfg : Cfg.t -> unit
val decl : Ast.decl -> unit
val funtab : Cfg.funtab -> unit
val coherent_loop : Cfg.Edge.t -> unit
val cloture : Cfg.t -> unit 
val separator : unit -> unit 
