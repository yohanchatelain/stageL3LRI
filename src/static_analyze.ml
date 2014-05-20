(* Static analyse *) 

open Ast
open Str

let iter n = for i=0 to n-1 do Printf.printf "x%d " i done 

let rec print = function
  |Dconstructor (s,i) -> Printf.printf "%s " s; iter i; print_newline ()  
  |Dfunction ((name,arg,e)::l) -> 
    Printf.printf "\n%s " name; (List.iter (Printf.printf "%s ") arg);
    Printf.printf "= "; (print_E e); print (Dfunction l);
  |Dfunction [] -> ()

and print_E = function
  |Evar (s) -> Printf.printf "%s" s
  |Econstr(s,[]) -> Printf.printf "%s[]" s
  |Econstr (s,x::pl) -> Printf.printf "%s[" s; (print_E x); List.iter (fun s -> Printf.printf ","; print_E s) pl;
    Printf.printf "]"
  |Ecall (s,el) -> Printf.printf "%s"  s; List.iter
    (fun e -> Printf.printf " "; print_E e) el
  |Elet (s,e1,e2) -> Printf.printf "let %s = " s; print_E e1; 
    Printf.printf " in \n"; print_E e2 
  |Eif (e1,e2,e3) -> 
    Printf.printf " if "; print_E e1; Printf.printf " then "; print_E e2;
    Printf.printf " esle "; print_E e3
  |Ematch (e,bl) -> Printf.printf "match "; print_E e ; Printf.printf " with\n"; 
 List.iter (fun s -> Printf.printf "|"; print_b s;print_newline ()) bl; Printf.printf "end\n"
  
and print_P = function
  |Pwild -> Printf.printf "_"
  |Pvar s -> Printf.printf "%s" s
  |Pconstr(s,[]) -> Printf.printf "%s[]" s
  |Pconstr (s,x::pl) -> Printf.printf "%s[" s; (print_P x); List.iter (fun s -> Printf.printf ","; print_P s) pl;
    Printf.printf "]"
  
and print_b = function
  |(p,e) -> print_P p; Printf.printf " -> "; print_E e 

(* check total application functions and well arity constructors *) 
 
let print_error a b c = function
  |Ecall _ -> Format.open_hbox();Format.eprintf "\nError :\n@[<h>%s is previously called with %i argument(s) instead of %i\nOnly total applications are allowed\n\n@]" a b c ;
exit 1
  |Econstr _ -> Printf.eprintf "\nError :\nThe constructor %s expects %i
argument(s),\nbut is applied here to %i argument(s)\n\n" a b c; exit 1
  |_ -> ()

let check_tuple_cons name = 
(Str.string_match (Str.regexp "Tuple-[1-9][0-9]*") name 0)

exception Unknow_arg of string * string

let check_arity list = 
  let h = Hashtbl.create 5 in
  let rec aux = function
    |[] -> ()
    |(Dconstructor (name,arity))::l ->
      Hashtbl.add h name arity; aux l
    |(Dfunction fl)::l ->
      List.iter (function name,args,_ -> 
	Hashtbl.add h name (List.length args)) fl;
      aux l;
      List.iter (fun (_,_,e) -> aux2 (e::[])) fl

  and aux2 = function  
    |[] -> ()   
    |(Evar _)::l-> aux2 l
    |(Elet(_,e1,e2))::l -> aux2 (e1::(e2::l))
    |(Eif(e1,e2,e3))::l -> aux2 (e1::(e2::(e3::l)))
    |(Ematch(e,bl))::l -> (aux2 (e::l)); (aux3 bl)
    |(Econstr (name,el) as ec)::l ->      
      begin
	if not (check_tuple_cons name) then	
	  begin
	    try
	      begin
		let real_length = (Hashtbl.find h name) in
		let length = List.length el in
		if real_length = length then
		  aux2 l
		else
		  print_error name real_length length ec
	      end
	    with
	    |Not_found -> 
	      raise (Unknow_arg ("constructor",name)) 	      
	  end
	else
	  aux2 l
      end      
    |(Ecall (name,el) as ec )::l  -> 
      begin
	try
	  let real_length = Hashtbl.find h name in 
	  let length = List.length el in 
	  begin
	  if real_length = length then
	    aux2 l
	  else
	    print_error name real_length length ec;
	  end	
	with
	|Not_found -> 
	  Hashtbl.add h name (List.length el);
	  aux2 el
      end
  and aux3 = function
    |[] -> ()
    |(p,e)::l -> (aux4 p); (aux2 (e::[])); aux3 l

  and aux4 = function
    Pconstr (s,pl) -> 
      begin
	if not (check_tuple_cons s) then	
	  begin
	    try
	      let real_length = Hashtbl.find h s in
	      let length = List.length pl in
	      begin
		if real_length = length then
		  List.iter (aux4) pl
		else
		  print_error s real_length length (Econstr ("",[]))	 
	      end
	    with
	    |Not_found -> raise (Unknow_arg ("constructor",s)) 
	  end
      end
    |_ -> ()
     
  in
  aux list

