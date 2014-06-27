open Ast
open Substitution

module E = struct 
  type t = subst
  let compare s1 s2 = Substitution.compare s1 s2 
end

module Edge = Set.Make(E)

type graph = Edge.t array array
type funtab = (string,int) Hashtbl.t

type t = {graph : graph ; funtab : funtab}

let equal cfg1 cfg2 = 
  let g1 = cfg1.graph in
  let g2 = cfg2.graph in
  let n = Array.length g1 in
  let bool = ref true in
  for i=0 to n-1 do
    for j=0 to n-1 do
      bool := Edge.equal g1.(i).(j) g2.(i).(j) && !bool
    done;
  done;
  !bool

let get_graph g = g.graph
let get_funtab g = g.funtab 

let reset = ref (-1)

let add = 
  let r = reset in
  fun tab name -> 
    if not (Hashtbl.mem tab name ) then
	begin 
     	  incr r; 
     	  Hashtbl.add tab name !r 
	end 

let find = Hashtbl.find

let symtab g = Hashtbl.fold (fun k v acc -> (v,k)::acc) g.funtab []

(* List product *)
let combine =
  let rec aux1 = function
    |[] -> []
    |hd::[] -> hd
    |hd::tl -> aux2 (hd,(aux1 tl))

  and aux2 = function
    |l1,[] -> l1
    |[],l2 -> assert false
    |l1,l2 ->
      List.fold_left (fun acc e1 ->
	(List.fold_left (fun acc e2 ->
	  (Substitution.union e1 e2)::acc
	 ) [] l2)@acc
      ) [] l1
  in
  aux1

(* Replace each arc labeled by a sum with as many as arcs as summands  *)
(* Appendix B p.28 *)
let split_sum e =
  let (es,eo) = Edge.partition (Env.exists (fun _ v -> Term.is_sum v)) e in
  let splt = Edge.fold (fun subs acc ->
    let (sum,other) = Env.partition (fun _ v -> Term.is_sum v) subs in
    let split = Env.fold (fun k v acc ->
      (List.map (fun t -> Env.add k t other) (Term.list_of_sum v))::acc)
      sum [] in
    let l = List.fold_left (fun acc e -> Edge.add e acc) 
      Edge.empty (combine split) in
    Edge.union acc l) 
    es Edge.empty in
  Edge.union splt eo

(* Collect anonymus function,  *)
(* i.e., function not declared with "and"  *)
(* They must be declared previously *)
let anonymus =
  let rec expr = function
    | Elet (_,e1,e2) -> expr e1 @ expr e2
    | Eif (e1,e2,e3) -> expr e1 @ expr e2 @ expr e3 
    | Ematch (e,bl) ->
       expr e @ List.fold_left (fun acc b -> acc @ branch b ) [] bl
    | Econstr (_,el) ->
      List.fold_left (fun acc e -> acc @ expr e) [] el
    | Ecall (name,el) ->
      List.fold_left (fun acc e -> acc @ expr e) [name] el
    | _ -> []
  and branch (p,e) =
    expr e
in
  expr

let create defunl =
  let tab = Hashtbl.create 7 in
  List.iter (fun (name,_,_) -> add tab (name)) defunl;
  List.iter (fun (_,_,e) -> List.iter (add tab) (anonymus e)) defunl;
  let n = Hashtbl.length tab in
  let m = Array.make_matrix n n Edge.empty in
  let cl = defun defunl in
  List.iteri (fun i cl -> 
    List.iter (fun (s,subst) -> 
      let j = find tab s.name in
      m.(i).(j) <-  split_sum (Edge.add subst m.(i).(j));
    ) cl 
  ) cl;
  reset := (-1);
  {graph = m; funtab = tab}

let product l1 l2 = 
  Edge.fold (fun sub acc ->
    Edge.union 
      (Edge.fold (fun s acc' -> Edge.add (Substitution.compose sub s) acc')
	 l2 Edge.empty) acc)
  l1 Edge.empty 
  
let union g1 g2 = 
  let g1' = g1.graph in
  let g2' = g2.graph in
  let n = Array.length g1' in
  let m = Array.make_matrix n n Edge.empty in
  for i=0 to n-1 do
    for j=0 to n-1 do
      m.(i).(j) <- Edge.union g1'.(i).(j) g2'.(i).(j)
    done;
  done;
  { graph = m; funtab = g2.funtab }  

let computation g1 g2 = 
  let n = Array.length (get_graph g1) in
  let m = Array.make_matrix n n Edge.empty in
  for i=0 to n-1 do
    for j=0 to n-1 do 
      for k=0 to n-1 do
	let l1 = (get_graph g1).(i).(k) in
	let l2 = (get_graph g2).(k).(j) in
	let p = product l1 l2 in
	  m.(i).(j) <- Edge.union p m.(i).(j)
      done;
    done;
  done;
  {graph = m ; funtab = g1.funtab }

let separator () = 
  for i = 0 to 50 do Format.printf "_" done;
  Format.printf "@\n@\n"

let print_step i =
  fun p gn ->  
    Format.printf "G%d@\n@\n" i;
    p gn;
    separator ()

let cloture p g =
  let count = ref (-1) in
  let gn = ref g in
  let gn_1 = ref (computation g g) in 
  gn_1 := union !gn !gn_1;
  while  not (equal !gn !gn_1) do
    incr count;
    if Options.print_step then
     print_step !count p !gn;
    gn := !gn_1;
    gn_1 := union !gn (computation !gn g);
  done;
  if Options.print_step then separator ();
  !gn

let self_loop g =
  let m = get_graph g in
  let n = Array.length m in
  let l = ref Edge.empty in
  for i = 0 to n-1 do
    l := Edge.union m.(i).(i) (!l)
  done;
  !l

let loop g =
  let m = get_graph g in
  let n = Array.length m in
  let l = ref Edge.empty in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      l := Edge.union m.(i).(j) (!l)
    done;
  done;
  !l

(* Self loop coherent in sense of Lemma 1.20 *)
let coherent_self_loop g =
  Edge.filter (fun subs ->
    Env.for_all (fun k v ->
      Env.exists (fun k' v' ->
	k=k' && Term.check_coherent (v,v')
      ) (Substitution.compose subs subs)
    ) subs
  ) (self_loop g)
