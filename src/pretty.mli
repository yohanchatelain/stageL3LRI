open Substitution

val term: Term.t -> unit
val program: Ast.program -> unit
val call: (sym*Term.t Env.t) list -> unit
val cfg : Cfg.t -> unit
val separator : unit -> unit 
