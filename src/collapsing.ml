open Term

let b = Options.b
let d = Options.d

let rec nb_destr  = function
  | Tvar _ -> 0
  | Tdestruc (_,t) 
  | Tproject (_,t) -> 1 + nb_destr t
  | Tsum l
  | Tuple l -> 
    List.fold_left (fun a t -> Pervasives.max a (nb_destr t)) 0 l
  | Tapprox (_,t)
  | Tcons (_,t) -> nb_destr t
  | Terror -> Term.error ()

let collapse_D t =
  let rec up i = function
    | Tsum l -> sum (List.map (up i) l)
    | Tcons (s,t) when i > 0 -> cons s (up (i-1) t)
    | Tuple l when i > 0 -> tuple (List.map (up (i-1)) l)
    | Tapprox (i',ta) when i > 0 -> approx i' (down ta) 
    | t when i = 0 -> nf (down (approx (Infinity.Int 0) t))
    | Tvar _ as t -> t 
    | Tdestruc _ 
    | Tproject _ as t when i > 0 -> down t
    | t -> Term.error ()

  and down = function
    | Tsum l -> sum (List.map down l)
    | Tapprox (i',t) -> approx i' (down t)
    | t when nb_destr t <= d -> t
    | Tvar _ as t -> t 
    | Tdestruc (_,t') 
    | Tproject (_,t') as t when nb_destr t > d 
	-> approx (Infinity.Int (-1)) (down t')
    | t -> Term.error ()
  in
  up d t

let ceil = function
  | Infinity.Infinity -> Infinity.Infinity
  | Infinity.Int w -> 
    begin
      if w < -b then
	Infinity.Int (-b)
      else if w >= b then
	Infinity.Infinity
      else
	Infinity.Int w
    end

let rec collapse_B = function
  | Tcons (s,t) -> cons s (collapse_B t)
  | Tuple l -> tuple (List.map (collapse_B) l)
  | Tsum l -> sum (List.map (collapse_B) l) 
  | Tapprox (i,t) -> approx (ceil i) t
  | Tdestruc _ 
  | Tproject _    
  | Tvar _ as t -> t
  | Terror -> assert false

let collapse t =  collapse_B (collapse_D t)
