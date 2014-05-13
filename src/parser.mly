
/* Analyseur syntaxique pour SCT */

%{
  open Ast
%}

%token <int> INTEGER
%token <string> LIDENT
%token <string> UIDENT
%token IF THEN ELSE LET REC IN MATCH WITH CONSTRUCTOR END AND
%token LPAREN RPAREN EQ ARROW BAR COMMA UNDERSCORE EOF

/* Définitions des priorités et associativités des tokens */


/* Point d'entrée de la grammaire */
%start prog

/* Type des valeurs retournées par l'analyseur syntaxique */
%type <Ast.program> prog

%%

prog:
| dl = decl*; EOF  { dl }
;

decl:
| CONSTRUCTOR; id = UIDENT; n = INTEGER
  { Dconstructor (id, n) }
| LET; REC; l = separated_nonempty_list(AND, fundef)
   { Dfunction l}
;

fundef:
| id = LIDENT; args = LIDENT+; EQ; e = expr
   { (id, args, e) }
;

expr:
| e = simple_expr
    { e }
| id = LIDENT; el = simple_expr+;
    { Ecall (id, el) }
| id = UIDENT; el = simple_expr+;
    { Econstr (id, el) }
| IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { Eif (e1, e2, e3) }
| LET; x = LIDENT; EQ; e1 = expr; IN; e2 = expr
    { Elet (x, e1, e2) }
| MATCH; e = expr; WITH; BAR?; bl = separated_nonempty_list(BAR, branch); END
    { Ematch (e, bl) }
;

simple_expr:
| id = LIDENT
    { Evar id }
| id = UIDENT
    { Econstr (id, []) }
| LPAREN; e = expr; RPAREN
    { e }
| LPAREN; e = expr; COMMA; l = separated_nonempty_list(COMMA, expr); RPAREN
    { let id = Printf.sprintf "Tuple-%d" (1 + List.length l) in
      Econstr (id, e :: l) }
;

branch:
| p = pattern; ARROW; e = expr
    { p, e }
;

pattern:
| id = UIDENT; l = simple_pattern*
    { Pconstr (id, l) }
| p = simple_pattern 
   { p }
;

simple_pattern:
| id = LIDENT
    { Pvar id }
| UNDERSCORE
    { Pwild }
| LPAREN; p = pattern; RPAREN
  { p }
;

