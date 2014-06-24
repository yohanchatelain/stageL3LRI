
type vertex = string
module Edge : Set.S with type elt = Substitution.subst
type graph = Edge.t array array
type funtab = (string,int) Hashtbl.t

type t = {graph : graph ; funtab : funtab}

val get_graph : t -> graph
val get_funtab : t -> funtab
val symtab : t -> (int * string) list 
val create: Ast.defun list -> t 
val computation : t -> t -> t 
val cloture : (t -> unit) -> t -> t
val coherent_self_loop : t -> Edge.t
  
