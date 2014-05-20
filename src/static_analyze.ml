(* Static analyse *)

open Format
open Ast
open Str

(* check total application functions and well arity constructors *)

let bad_arity a b c =
  eprintf "@[<hov 2>Error:@\nThe constructor/function %s expects %i argument(s),@ but is applied here to %i argument(s)@]@." a b c; exit 1

let unbound_var x =
  eprintf "unbound variable %s@." x; exit 1

let arity =
  Hashtbl.create 5

let get_arity name =
  try
    Scanf.sscanf name "Tuple-%d" (fun n -> n)
  with Scanf.Scan_failure _ | End_of_file ->
  try
    Hashtbl.find arity name
  with Not_found ->
    eprintf "unknown constructor/function %s@." name; exit 1

module S = Set.Make(String)

let check_arity list =
  let rec expr vars = function
    | Evar x ->
      if not (S.mem x vars) then unbound_var x
    | Elet (x,e1,e2) -> expr vars e1; expr (S.add x vars) e2
    | Eif (e1,e2,e3) -> expr vars e1; expr vars e2; expr vars e3
    | Ematch (e,bl) -> expr vars e; List.iter (branch vars) bl
    | Econstr (name,el)
    | Ecall (name,el)  ->
      let n = get_arity name in
      let m = List.length el in
      if m <> n then bad_arity name n m;
      List.iter (expr vars) el
  and branch vars (p, e) =
    let vars = pattern vars p in
    expr vars e
  and pattern vars = function
    | Pconstr (name, pl) ->
      let n = get_arity name in
      let m = List.length pl in
      if m <> n then bad_arity name n m;
      List.fold_left pattern vars pl
    | Pvar x ->
      S.add x vars
    | Pwild ->
      vars
  in
  let decl = function
  | Dconstructor (name, n) ->
      Hashtbl.add arity name n
  | Dfunction fl ->
      List.iter (fun (name, args, _) ->
	Hashtbl.add arity name (List.length args)) fl;
      List.iter (fun (_,args,e) ->
        let vars = List.fold_right S.add args S.empty in
        expr vars e)
        fl
  in
  List.iter decl list

