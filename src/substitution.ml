open Ast
open Term  
open Infinity

type sym = { name : string; args : string list; id : int}
module Env = Map.Make(String)
type subst = sym * Term.t Env.t 

let sym = Hashtbl.create 5

let mk_sym =
  let r = ref 0 in
  fun ?args name -> incr r;
    match args with
    |Some l ->       
      let s = {name = name; args = l ; id = !r } in
      Hashtbl.add sym name s; s
    |None ->
      let x = Hashtbl.find sym name in
      {name = name; args = x.args; id = !r }
 
let union = Env.merge (fun k x y ->  
  match x,y with
  |Some m1, Some m2 -> Some m2
  |m, None 
  |None,m ->  m)

let rec subst x t' = function 
  |Tvar y when x=y -> t'
  |Tvar _ as t -> t
  |Tcons (s,t) -> cons s (subst x t' t)
  |Tuple tl -> tuple (List.map (subst x t') tl)
  |Tdestruc (s,t) -> destruct s (subst x t' t)
  |Tproject (i,t) -> project i (subst x t' t)
  |Tsum tl -> sum (List.map (subst x t') tl)
  |Tapprox (i,t) -> approx i (subst x t' t)
  |Terror -> error ()

let pattern sub t p =
  let rec aux sub acc = function
    |Pvar s -> 
      Env.add s (acc t) sub
    |Pconstr (s,pl) (*when List.length pl > 1*) -> 
      let i = ref (-1) in
      List.fold_left (fun sacc p ->
	incr i;
	let acc = (fun t -> acc (project (!i) (destruct s t))) in
	aux sacc acc p) sub pl
     
    |Pwild -> sub
  in
  aux sub (fun t -> t) p 
        
let rec create sub = function
  |Elet (s,e1,e2) -> 
    let (t1,cl1) = create sub e1 in 
    let sub = Env.add s t1 sub in
    let (t2,cl2) = create sub e2 in
    t2, List.rev_append cl1 cl2
  |Eif (c,e1,e2) -> 
    let (t,cl) = create sub c in
    let (t1,cl1) = create sub e1 in
    let (t2,cl2) = create sub e2 in
    sum (t::t1::t2::[]),List.rev_append cl2 (List.rev_append cl  cl1) 
  |Ecall (s,el) ->
    let s = mk_sym s in
    let (cl,sb) = List.fold_left2 
      (fun (l,sub) a e -> 
        let (te,cle) = create sub e in
	(l@cle,Env.add a te sub))
      ([],sub) s.args el in
    let (sb,_) = Env.partition (fun a _ -> List.mem a s.args) sb in
    approx Infinity (tuple []),(s,sb)::cl
  |Ematch (e,bl) ->
    let (tm,cl) = create sub e in
    let (t,c) = List.fold_left (fun (t,cl) (p,e) ->
      let sub = pattern sub tm p in
      let (t',cl') = create sub e in
      t'::t,cl@cl') ([tm],cl) bl
    in sum (List.rev t),c
  |Econstr (s,el) -> 
    let (l,cl) = List.fold_left (fun (t,cl) e -> 
      let (t',cl') = create sub e in
      t'::t,List.rev_append cl cl') ([],[]) el in
    (cons s (tuple (List.rev l))),cl
  |Evar s -> (subst s (var s) (Env.find s sub),[])

(* t1 ° t2 
let compose sub1 sub2 = 
  Env.map (fun k t -> Env.find k sub2 )*)
  

let call list = 
  let aux = function
    |Dconstructor _ -> []
    |Dfunction l ->
      List.iter (fun (n,a,e) -> ignore (mk_sym ~args:a n)) l;
      List.fold_left (fun acc (n,a,e) -> 
	let sub = List.fold_left 
	  (fun acc a -> Env.add a (var a) acc) Env.empty a in
	let (_,cl) = create sub e in 
      cl::acc) [] l
  in
  List.fold_left (fun acc f -> acc@(aux f)) [] list 

