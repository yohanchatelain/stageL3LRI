open Format 

(* Terms and reduction  *)
(* Definiton 1.1 p.5 *)
type t =
| Tvar of string
| Tcons of string * t
| Tuple of t list
| Tdestruc of string * t
| Tproject of int * t
| Tsum of t list
| Tapprox of Infinity.t * t
| Terror

let error t = failwith t

let unique = 
  let rec aux acc = function
    | [] -> acc
    | hd::tl -> aux (if List.mem hd acc then acc else hd::acc) tl  
  in
  aux []
    
let is_sum = function 
  | Tsum _ -> true 
  | _ -> false

let list_of_sum = function
  | Tsum l -> l
  | _ -> assert false 

(* Smart constructors definition 1.2 p.6 *)

let var x = Tvar x
  
let rec sum  = function 
  | hd::[] -> hd
  | l -> 
    let rec aux acc = function
      | [] -> Tsum (unique acc)
      | (Tsum l)::tl -> aux (acc@l) tl
      | hd::tl -> aux (acc@[hd]) tl
    in
    aux [] l 
      
let tuple l =
  let rec chck_sum pred = function
    | [] -> Tuple pred 
    | (Tsum l)::tl -> sum
      (List.fold_left 
	(fun acc t -> acc@[(chck_sum [] (pred@[t]@tl))] ) [] l)
    | hd::tl -> chck_sum (pred@[hd]) tl
  in
  chck_sum [] l

let rec cons s = function
  | Tuple [t] -> cons s t
  | Tsum l -> sum (List.map (fun t -> cons s t) l)
  | t -> Tcons (s, t)

let rec approx i = function
  | Tsum l -> sum (List.map (fun t -> approx i t ) l)
  | Tcons(s,t) -> approx (Infinity.add_int 1 i) t
  | Tapprox (j,t) -> approx (Infinity.add i j) t
  | Tuple l when List.length l > 0 ->
    sum (List.map (fun t -> approx (Infinity.add_int 1 i) t) l)
  | t -> Tapprox (i,t)

let rec destruct s = function
  | Tuple [l] -> destruct s l 
  | Tuple _  -> failwith "destruct tuple"
  | Tsum l -> Tsum (List.map (destruct s) l)
  | Tcons(s',t) when s = s' -> t
  | Tapprox(i,t) -> approx (Infinity.add_int (-1) i) t
  | t -> Tdestruc(s,t)

let rec project i = function
  | Tsum l -> sum (List.map (project i) l)
  | Tuple l -> 
    begin
      try 
	List.nth l i 
      with
      |Failure _ ->  failwith "project i > n"  
    end
  | Tcons _  -> failwith "project cons"
  | Tapprox (j,t) -> approx (Infinity.add_int (-1) j) t
  | t -> Tproject (i,t)

(* Length |d| of simple terms  *)
(* is the numbers of destructors it contains *)
(* Lemma 1.5 p.7 *)
let rec nb_destr  = function
  | Tvar _ -> 0
  | Tdestruc (_,t) 
  | Tproject (_,t) -> 1 + nb_destr t
  | Tsum l
  | Tuple l -> List.fold_left (fun a t -> a + nb_destr t) 0 l
  | Tapprox (_,t)
  | Tcons (_,t) -> nb_destr t
  | Terror -> error "nb_destr"

(* Normal form of term *)
(* Use in collapsing p.11 *)
let rec nf = function
  | Tvar _ as t -> t
  | Tcons (s,t) -> cons s (nf t)
  | Tsum l -> sum (List.map nf l)
  | Tuple l -> tuple (List.map nf l)
  | Tdestruc (s,t) -> destruct s (nf t)
  | Tproject (i,t) -> project i (nf t)
  | Tapprox (i,t) -> approx i (nf t)
  | Terror -> error "nf"

(* Substitution t[x:= t'] *)
let rec subst x t' = function
  | Tvar y when x = y -> t'
  | Tvar _ as t -> t
  | Tcons (s,t) -> cons s (subst x t' t)
  | Tuple tl -> tuple (List.map (subst x t') tl)
  | Tdestruc (s,t) -> destruct s (subst x t' t)
  | Tproject (i,t) -> project i (subst x t' t)
  | Tsum tl -> sum (List.map (subst x t') tl)
  | Tapprox (i,t) -> approx i (subst x t' t)
  | Terror -> error "subst"

(* b is_suffix of d *)
let rec is_suffix = function
  | Tvar x, Tvar y when x = y -> true
  | _ ,Tvar _ -> false
  | Tdestruc (s,t), Tdestruc (s',t') ->
    is_suffix (if s = s' then t,t' else t,Tdestruc (s',t'))
  | Tproject (i,t) , Tproject (i',t') ->
    is_suffix (if i = i' then (t,t') else (t,Tproject (i',t')))
  | Tdestruc (_,t) , d 
  | Tproject (_,t) , d -> is_suffix (t,d)
  | _ -> false

(* Preorder on the set of terms *)
(* Definition 1.6 p.7 *)
(* Definition A.1 p.25 *)
let rec less = function
  | t,t' when t = t' -> true
  | t, Tapprox (Infinity.Int 0,t') when t = t' -> true 
  | Tsum l, Tsum l' -> 
    List.for_all (fun t -> List.exists (fun t' -> less (t,t') ) l') l
  | Tdestruc (_,t),Tapprox (i,t') 
  | Tproject (_,t),Tapprox (i,t') 
    when (Infinity.compare (Infinity.Int 0) i) > 0 -> true
  | Tcons (x,t),Tcons (y,t') when x = y -> less (t,t')
  | Tuple l, Tuple l' when List.length l = List.length l' ->
    List.for_all2 (fun u v -> less (u,v)) l l'
  | Tapprox (i,t) ,Tapprox (i',t') -> 
    is_suffix (t',t) && Infinity.compare (Infinity.add_int (nb_destr t) i) 
    (Infinity.add_int (nb_destr t') i') < 0
  | t,(Tapprox _ as a) -> less ((approx (Infinity.Int 0) t),a) 
  | _ -> false

let subterm =
  let rec aux acc  = function 
    | Tvar x -> acc
    | Tcons (_,t) -> aux acc t
    | Tsum l 
    | Tuple l -> 
      List.fold_left (fun acc t -> (aux [] t)@acc) [] l
    | Tapprox (_,t) -> aux acc t
    | Tproject (_,t')
    | Tdestruc (_,t') as t -> aux ((approx (Infinity.Int 0) t)::acc) t' 
    | Terror -> error "decreasing_parameter"
  in
  aux []

let rec is_subterm t = function
  | t' when t = t' -> true
  | Tvar x -> false
  | Tcons (_,t')
  | Tdestruc (_,t')
  | Tproject (_,t')
  | Tapprox (_,t') -> is_subterm t t'
  | Tuple l 
  | Tsum l -> List.exists (is_subterm t) l
  | Terror -> error "is_subterm"

(* Test if t (epsilon) is a decreasing parameter *)
(* Definition 2.5 p.15 *)
(* Describe at the end of the page 29 *)
let test v = function
  | Tapprox (_,t),Tapprox (i,t') 
    when (Infinity.compare (Infinity.Int 0) i) > 0 
      -> is_subterm t v && t = t'
  | _ -> false 

(* Compatibility of terms *)
(* Definition 1.19 p.12 *)
(* Lemma B.1 p.29 *)
let rec check_coherent = function
  | t,t' when t = t' -> true
  | Tcons (s,t), Tcons (s',t') when s = s' -> check_coherent (t,t')    
  | Tuple l, Tuple l' when List.length l = List.length l' ->
    List.fold_left2 (fun acc t t' -> acc && (check_coherent (t,t')))
      true l l' 
  | Tdestruc (s,t), Tdestruc (s',t') when s = s' -> check_coherent (t,t')
  | Tproject (i,t), Tproject (i',t') when i = i' -> check_coherent (t,t')
  | Tapprox (i,t), Tapprox (i',t') -> is_suffix (t,t') || is_suffix (t',t)
  | Tcons (s,t) , Tsum l -> check_coherent (t,Tsum l)
  | Tuple l, Tsum l'-> 
    List.for_all (fun t -> check_coherent (t,Tsum l')) l
  | Tsum l, Tsum l' ->
    List.exists (fun t ->
      List.exists (fun t' -> check_coherent (t,t')) l') l
  | _ -> false
