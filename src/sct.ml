
open Ast
open Substitution

exception Error of string
	       
let compute = 
  let rec aux acc = function
    | [] -> acc
    | (Dconstructor _)::tl -> aux acc tl
    | (Dfunction l)::tl -> aux ((Cfg.create l)::acc) tl
  in
  aux []

let is_decreasing cfg = 
  let coh_sloop = Cfg.coherent_self_loop cfg in
  Cfg.Edge.for_all (fun sub -> 
    Env.exists (fun k v -> 
      let ldrec = Term.decreasing_parameter v in
      List.exists (fun t -> 
	Env.exists (fun _ v -> 
	  Term.test v (t,(Collapsing.collapse (Term.subst k v t)))) sub)
	ldrec) sub) coh_sloop

let prog p =
  let lclot = List.map (Cfg.cloture Pretty.cfg) (compute p) in
  
  if Options.print_coherent_self_loop then 
    List.iter (fun g -> Pretty.coherent_loop (Cfg.coherent_self_loop
    g)) lclot;

  List.iter (fun cfg -> Format.printf (
    (* Pretty.funtab (Cfg.get_funtab cfg); *)
    if (is_decreasing cfg) then
      "Functions terminate@\n" 
    else 
      "Sct can not answer@\n")) 
    lclot;
  
  Format.printf "@\n"

 
