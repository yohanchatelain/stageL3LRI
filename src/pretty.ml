open Format
open Ast
open Term
open Substitution

let iter n = for i = 0 to n-1 do printf "x%d " i done

let list p s = function
  | [] -> printf "%s" s
  | h::t -> printf "%s[" s; p h; List.iter (fun s -> printf ","; p s) t;
    printf "]"
      
let tuple p l = 
  printf "(";
  let rec aux = function
    | [] -> printf ")"
    | hd::[] -> p hd ; printf ")" 
    | hd::tl -> p hd; printf ","; aux tl
  in
  aux l

let sum p l =
  printf "@[<hv>";   
  let rec aux = function
    | [] -> ()
    | hd::[] -> p hd;
    | hd::tl -> p hd; printf "@ + "; aux tl 
  in
  aux l; printf "@]"
    
let rec expr = function
  | Evar (s) -> printf "%s" s
  | Econstr(s,t) -> list expr s t
  | Ecall (s,el) -> printf "%s"  s; List.iter
    (fun e -> printf " "; expr e) el
  | Elet (s,e1,e2) -> printf "let %s = " s; expr e1;
    printf " in @\n"; expr e2
  | Eif (e1,e2,e3) ->
    printf "if "; expr e1; printf " then @\n "; expr e2;
    printf "@\nelse@\n "; expr e3
  | Ematch (e,bl) ->
    printf "@[<v>match "; expr e ; printf " with@\n";
    List.iter branch bl; printf "end@]"

and pattern = function
  | Pwild -> printf "_"
  | Pvar s -> printf "%s" s
  | Pconstr(s,t) -> list pattern s t

and branch (p,e) =
  printf "| "; pattern p; printf " -> "; expr e; printf "@\n"

let fundef (name,arg,e) =
  printf "@[<hov 2>%s " name; List.iter (fun x -> printf "%s " x) arg;
  printf "=@\n"; expr e; printf "@]@\n"

let decl = function
  | Dconstructor (s,i) -> printf "%s " s; iter i; printf "@\n"
  | Dfunction l -> printf "@\n"; List.iter fundef l

let program = List.iter decl

let rec term = function
  | Tvar s -> printf "%s" s
  | Tcons (s,Tvar s') -> printf "%s "s; printf "%s" s'  
  | Tcons (s,t) -> printf "%s" s; term t
  | Tuple t -> tuple term t
  | Tdestruc (s,t) -> printf "%s¯" s; term t
  | Tproject (i,t) -> printf "π_%d" i; term t
  | Tsum t -> sum term t
  | Tapprox (i,t) -> printf "<"; Infinity.print i; printf ">"; term t
  | Terror -> eprintf "error"

let sym x = 
  printf "%s:" x.name;
  List.iter (fun s -> printf " %s" s) x.args

let subst (s,m) =
  let l = Env.bindings m in
  printf "@[<v 2>%s:@\n[" s.name; 
  let (ss,v) = List.hd l in
  printf "%s := " ss; term v;
  List.iter (fun (k,t) -> printf " ; ";printf "%s := " k; term t )
  (List.tl l);
  printf "]@]@\n@\n"

let call = List.iter subst

let cfg g =
  let t = List.sort (fun (i,_) (j,_) -> Pervasives.compare i j )
  (Cfg.symtab g) in 
  Array.iteri (fun i l ->
    printf "@[<v 2>%s :@\n" ((List.assoc i t)) ;
    Array.iteri (fun j c ->
      printf "@[<v>-%s : " ((List.assoc j t));	     
      printf "@[<v>";
      if Cfg.Edge.is_empty c then 
	printf "Ø";
      Cfg.Edge.iter (fun c ->
	let l = Env.bindings c in
       	let (ss,v) = List.hd l in
	printf "[%s := " ss; term v;
	List.iter (fun (k,t) -> printf " ; ";printf "%s := " k; term t )
	  (List.tl l);     	  
	printf "]@ ") c;
      printf "@]";
      printf "@]@\n") l ; printf "@]@\n" ) (Cfg.get_graph g) 
    
let decl = function
  | Dconstructor _ ->
    ()
  | Dfunction l ->
    let gl = Cfg.create l in
    cfg gl

let funtab =  Hashtbl.iter (fun k _ -> printf "%s " k)

let coherent_loop e = 
  printf "@[<v>";
  Cfg.Edge.iter (fun c ->
    let l = Env.bindings c in
    let (ss,v) = List.hd l in
    printf "[%s := " ss; term v;
    List.iter (fun (k,t) -> printf " ; ";printf "%s := " k; term t )
      (List.tl l);     	  
    printf "]@,") e;
  printf "@]"


let separator () = for i = 0 to 50 do printf "_" done; printf "@.\n\n"

let cloture g = 
  printf "G+ :@\n@\n";
  cfg g;
  separator ()

