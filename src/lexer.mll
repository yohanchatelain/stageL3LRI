
(* Analyseur lexical pour SCT *)

{
  open Lexing
  open Parser

  exception Lexing_error of string

  (* tables des mots-clés *)
  let kwd_tbl =
    ["if", IF; "then", THEN; "else", ELSE; "in", IN;
     "let", LET; "rec", REC; "match", MATCH; "with", WITH; "end", END;
     "constructor", CONSTRUCTOR; "and" ,AND
    ]

  let id_or_kwd =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
    fun s -> try Hashtbl.find h s with _ -> LIDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']
let letter = lower | upper | digit | '_'
let lident = lower letter*
let uident = upper letter*
let integer = ['0'-'9']+
let space = [' ' '\t']

rule token = parse
  | '\n'    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | lident as id { id_or_kwd id }
  | uident as id { UIDENT id }
  | '='     { EQ }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | ','     { COMMA }
  | '|'     { BAR }
  | '_'     { UNDERSCORE }
  | "->"    { ARROW }
  | "(*"    { comment lexbuf }
  | integer as s { INTEGER (int_of_string s) }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

(* note : les commentaires ne sont pas imbriqués *)
and comment = parse
  | "*)"    { token lexbuf }
  | _       { comment lexbuf }
  | eof     { raise (Lexing_error "unterminated comment") }


