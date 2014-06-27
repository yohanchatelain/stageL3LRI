open Term

let b = Options.b
let d = Options.d

(* Collapse depth p.11 *) 
let collapse_D t =
  let rec up i = function
    | Tsum l -> sum (List.map (up i) l)
    | Tcons (s,t) when i > 0 -> cons s (up (i-1) t)
    | Tuple l when i > 0 -> tuple (List.map (up (i-1)) l)
    | Tapprox (i',ta) when i > 0 -> approx i' (down ta) 
    | Tdestruc _ 
    | Tproject _ as t when i >= 0 -> down t
    | t when i = 0 -> down (nf (approx (Infinity.Int 0) t))
    | t -> t

  and down = function
    | Tsum l -> sum (List.map down l)
    | Tapprox (i',t) -> approx i' (down t)
    | t when Term.nb_destr t <= d -> t
    | Tvar _ as t -> t     
    | Tdestruc (_,t') 
    | Tproject (_,t') as t when Term.nb_destr t > d 
	-> approx (Infinity.Int (-1)) (down t')
    | t -> t
  in
  up d t

(* Definition 1.14 p.10 *)
let ceil = function
  | Infinity.Infinity as i -> i
  | Infinity.Int w as i -> 
    begin 
      if w < -b then
	Infinity.Int (-b)
      else if w >= b then
	Infinity.Infinity
      else
	i
    end

(* Collapse weight  *)
(* top of page 11 *)
let rec collapse_B = function
  | Tcons (s,t) -> cons s (collapse_B t)
  | Tuple l -> tuple (List.map (collapse_B) l)
  | Tsum l -> sum (List.map (collapse_B) l) 
  | Tapprox (i,t) -> approx (ceil i) t
  | Tdestruc _ 
  | Tproject _    
  | Tvar _ as t -> t
  | Terror -> failwith "collapse weight"

(* Definition 1.18 p.12 *)
let collapse t =  collapse_B (collapse_D t)
