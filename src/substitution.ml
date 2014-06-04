open Ast
open Term
open Infinity

module Env = Map.Make(String)
type subst = Term.t Env.t

type sym = { name : string; args : string list; id : int}
type call = sym * Term.t Env.t

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

let rec pattern sub t = function
  | Pvar s ->
    Env.add s t sub
  | Pconstr (s, pl) ->
    let i = ref (-1) in
    let t = destruct s t in
    List.fold_left (fun sub p -> incr i; pattern sub (project !i t) p) sub pl
  | Pwild -> sub

let rec expr sub = function
  |Elet (s,e1,e2) ->
    let (t1,cl1) = expr sub e1 in
    let sub = Env.add s t1 sub in
    let (t2,cl2) = expr sub e2 in
    t2, List.rev_append cl1 cl2
  |Eif (c,e1,e2) ->
    let (t,cl) = expr sub c in
    let (t1,cl1) = expr sub e1 in
    let (t2,cl2) = expr sub e2 in
    sum (t::t1::t2::[]),List.rev_append cl2 (List.rev_append cl  cl1)
  |Ecall (s,el) ->
    let s = mk_sym s in
    let (cl,sb) = List.fold_left2
      (fun (l,sub) a e ->
        let (te,cle) = expr sub e in
	(l@cle,Env.add a te sub))
      ([],sub) s.args el in
    let (sb,_) = Env.partition (fun a _ -> List.mem a s.args) sb in
    approx Infinity (tuple []),(s,sb)::cl
  |Ematch (e,bl) ->
    let (tm,cl) = expr sub e in
    let (t,c) = List.fold_left (fun (t,cl) (p,e) ->
      let sub = pattern sub tm p in
      let (t',cl') = expr sub e in
      t'::t,cl@cl') ([tm],cl) bl
    in sum (List.rev t),c
  |Econstr (s,el) ->
    let (l,cl) = List.fold_left (fun (t,cl) e ->
      let (t',cl') = expr sub e in
      t'::t,List.rev_append cl cl') ([],[]) el in
    (cons s (tuple (List.rev l))),cl
  |Evar s -> (subst s (var s) (Env.find s sub),[])

(* t1 ° t2
let compose sub1 sub2 =
  Env.map (fun k t -> Env.find k sub2 )*)


let defun l =
  List.iter (fun (n,a,e) -> ignore (mk_sym ~args:a n)) l;
  List.fold_left (fun acc (n,a,e) ->
    let sub = List.fold_left
      (fun acc a -> Env.add a (var a) acc) Env.empty a in
    let (_,cl) = expr sub e in
    cl::acc) [] l

