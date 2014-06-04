type t =
| Tvar of string
| Tcons of string * t
| Tuple of t list
| Tdestruc of string * t
| Tproject of int * t
| Tsum of t list
| Tapprox of Infinity.t * t
| Terror
    
exception Error

let mtuple s = Str.string_match (Str.regexp "Tuple-[1-9][0-9]*") s 0

let rec list = function 
  | Terror 
  | Tsum _ 
  | Tvar _ -> raise Error 
  | Tcons (_,t)
  | Tdestruc (_,t)
  | Tproject (_,t)  
  | Tapprox (_,t) -> list t
  | Tuple tl -> tl 
  
  
let var x = Tvar x

let sum l = Tsum l

let tuple l = Tuple l

let cons s = function
  | t when mtuple s -> t
  | Tsum l -> Tsum (List.map (fun t -> Tcons (s, t)) l)
  | t -> Tcons (s, t)

let destruct s = function
  | Tsum l -> Tsum (List.map (fun t -> Tdestruc (s,t)) l)
  | Tcons(s',t) when s=s' -> t  
  | Tapprox(i,t) -> Tapprox  (Infinity.add_int (-1) i, t)
  | t -> Tdestruc(s,t)

let project i = function
 (* | Tdestruc (s,Tproject (i,Tdestruc (s',t))) when mtuple s -> 
	Tproject (i,(destruct s' (List.nth (list t) i))) *)
  | Tsum l -> Tsum (List.map (fun t -> Tproject(i, t)) l)
  | Tuple l -> List.nth l i 
  | Tapprox (j,t) -> Tapprox(Infinity.add_int (-1) j,t)
  | t -> Tproject (i,t)

let approx i = function
  | Tsum l -> Tsum (List.map (fun t -> Tapprox(i,t)) l)  
  | Tcons(s,t) -> Tapprox (Infinity.add_int 1 i,t)
  | Tapprox (j,t) -> Tapprox (Infinity.add i j,t)
  | Tuple l when List.length l > 0 -> 
    Tsum ( List.map (fun t -> Tapprox (Infinity.add_int 1 i,t)) l)
  | t -> Tapprox (i,t)

let error () = raise Error
