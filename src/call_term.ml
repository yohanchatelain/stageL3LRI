type infinity =
|Infinity 
|Int of int

let add_infinity = function
  |Infinity,_ | _,Infinity -> Infinity
  |Int i1,Int i2 -> Int (i1+i2)

let add_int n = function
  |Infinity -> Infinity
  |Int i -> Int (i+n)
 
let max_infinity = function
  |Infinity,_ | _,Infinity -> Infinity
  |Int i1,Int i2 -> Int (max i1 i2)

(* 0 if i1 = i2 , -1 if i1 < i2 , 1 if i1 > i2 *)
let compare_infinity = function
  |Infinity,_ -> -1
  |_,Infinity -> 1
  |Int i1, Int i2 -> compare i1 i2
 
type t = 
|Tvar of string
|Tcons of string * t list 
|Tdestruc of string * t 
|Tproject of int * t
|Tsum of t list
|Tapprox of infinity * t
|Terror 

exception Error 

let print_infnty = function
  |Infinity -> Printf.printf "∞"
  |Int i -> Printf.printf "%d" i

let print = function
  |Tvar s -> print_string s
  |Tcons (s,t) when Str.string_match (Str.regexp "Tuple-[1-9][0-9]*") s 0
      -> Printf.printf "%s" s; Printf.printf "("; print hd;
	List.iter (fun t -> Printf.printf ","; print t) tl
  |Tcons (s,t) -> Printf.printf "%s" s; print t
  |Tdestruc (s,t) -> Printf.printf "%s¯" s; print t
  |Tproject (i,t) -> Printf.printf "π_%d" i; print t
  |Tsum (hd::tl) -> print hd; 
    List.iter (fun t ->  Printf.printf " + ";print t) tl
  |Tapprox (i,t) -> 
    Printf.printf "<"; print_infnty i; Printf.printf ">"; print t
  |Terror -> raise Error

let rec reduction = function
  |Tcons (s,Tsum l) -> Tsum (List.map (fun t -> Tcons(s,reduction t)) l)
  |Tdestruc (s,Tsum l) -> 
    Tsum (List.map (fun t -> Tdestruc(s,reduction t)) l)
  |Tproject (i,Tsum l) -> 
    Tsum (List.map (fun t -> Tproject(i,reduction t)) l)
  |Tapprox (i,Tsum l) -> 
    Tsum (List.map (fun t -> Tapprox(i,reduction t)) l)
  |Tdestruc (s1,Tcons(s2,t)) when s1=s2 -> reduction t 
  |Tapprox (i,Tcons(s,t)) -> Tapprox (add_int 1 i,reduction t)
  |Tdestruc (s,Tapprox (i,t)) -> Tapprox (add_int (-1) i,reduction t)
  |Tapprox (i,Tapprox (j,t)) -> Tapprox (add_infinity i j ,reduction t)
  |Tproject (i,Ttuple l) -> try List.nth l i with Failure _ -> raise Error
  |Tapprox (i,Ttuple l) -> 
    TSum ( List.map (fun t -> Tapprox ((add_int 1 i), reduction t) l))
  |Tproject (i,Tapprox(j,t)) -> Tapprox((add_int (-1) j),reduction t) 
  |_ as t -> t

module Env = struct 
  type t = string*Call_term.t list
  let create = List.map (fun arg -> (arg,Tvar(arg)))
  let add = 
