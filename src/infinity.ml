type t =
 | Infinity
 | Int of int

let add x y = 
  match x, y with
  |Infinity,_ | _,Infinity -> Infinity
  |Int i1,Int i2 -> Int (i1+i2)

let add_int n = function
  |Infinity -> Infinity
  |Int i -> Int (i+n)

let max x y = 
  match x, y with
  |Infinity,_ | _,Infinity -> Infinity
  |Int i1, Int i2 -> Int (max i1 i2)

(* 0 if i1 = i2 , -1 if i1 < i2 , 1 if i1 > i2 *)
let compare x y = 
  match x, y with
  | Infinity, Infinity -> 0
  | Infinity,_ -> 1
  | _,Infinity -> -1
  | Int i1, Int i2 -> compare i1 i2

let print = function
  |Infinity -> Format.printf "∞"
  |Int i -> Format.printf "%d" i

