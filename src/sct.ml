
open Ast

exception Error of string

let decl = function
  | Dconstructor _ ->
    ()
  | Dfunction l ->
    let call = Substitution.defun l in
    List.iter Pretty.call call

let prog p =
  List.iter decl p
