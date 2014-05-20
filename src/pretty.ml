
open Format
open Ast

let iter n = for i = 0 to n-1 do printf "x%d " i done

let rec expr = function
  |Evar (s) -> printf "%s" s
  |Econstr(s,[]) -> printf "%s[]" s
  |Econstr (s,x::pl) ->
    printf "%s[" s; expr x; List.iter (fun s -> printf ","; expr s) pl;
    printf "]"
  |Ecall (s,el) -> printf "%s"  s; List.iter
    (fun e -> printf " "; expr e) el
  |Elet (s,e1,e2) -> printf "let %s = " s; expr e1;
    printf " in @\n"; expr e2
  |Eif (e1,e2,e3) ->
    printf "if "; expr e1; printf " then "; expr e2;
    printf " else "; expr e3
  |Ematch (e,bl) ->
    printf "@[<v>match "; expr e ; printf " with@\n";
    List.iter branch bl; printf "end@]"

and pattern = function
  |Pwild -> printf "_"
  |Pvar s -> printf "%s" s
  |Pconstr(s,[]) -> printf "%s[]" s
  |Pconstr (s,x::pl) ->
    printf "%s[" s; (pattern x); List.iter (fun s -> printf ","; pattern s) pl;
    printf "]"

and branch (p,e) =
  printf "| "; pattern p; printf " -> "; expr e; printf "@\n"

let fundef (name,arg,e) =
  printf "@[<hov 2>%s " name; List.iter (fun x -> printf "%s " x) arg;
  printf "=@\n"; expr e; printf "@]@\n"

let decl = function
  |Dconstructor (s,i) -> printf "%s " s; iter i; printf "@\n"
  |Dfunction l -> printf "@\n"; List.iter fundef l

let program = List.iter decl

