
open Ast
open Substitution

exception Error of string
	       
let decl = function
  | Dconstructor _ ->
    ()
  | Dfunction l ->
    let gl = Cfg.create l in
    Pretty.cfg gl;
    Pretty.separator ()

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
	Env.exists (fun _ v -> Term.test v t (Term.subst k v t)) sub)
	ldrec) sub) coh_sloop

let prog p =
  List.iter decl p;
  Format.printf "Coherent Loop :\n";
  let l = compute p in
  let lclot = List.map (Cfg.cloture Pretty.cfg) l in
  List.iter (fun g -> Cfg.Edge.iter 
    (fun e -> Env.iter (fun k v -> Format.printf "%s := " k;
      Pretty.term v;Format.printf " ; " ) e; Format.printf "\n")
  (Cfg.coherent_self_loop g); Format.printf "\n") lclot;
  Format.printf "  --------------\n";
  Format.printf " Is decreasing : \n";
  List.iter (fun cfg -> Format.printf "-%b\n" (is_decreasing cfg)) lclot;
  Format.printf "\n"
(* List.iter (fun g -> Pretty.cfg (Cfg.cloture Pretty.cfg g)) l *)
 
