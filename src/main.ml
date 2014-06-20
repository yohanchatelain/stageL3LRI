
(* Fichier principal du compilateur sct *)

open Format
open Options
open Lexing

(* Localise une erreur en indiquant la ligne et la colonne *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" source_file l (c-1) c

let () =
  
  (* On v�rifie que le nom du fichier source a bien �t� indiqu� *)
  if source_file = "" then begin
    eprintf "Aucun fichier � compiler\n@?";
    exit 1
  end;

  (* Ce fichier doit avoir l'extension .sct *)
  if not (Filename.check_suffix source_file ".sct") then begin
    eprintf "Le fichier d'entr�e doit avoir l'extension .sct\n@?";
    Arg.usage options usage;
     exit 1
  end;

  (* Ouverture du fichier source en lecture *)
  let f = open_in source_file in

  (* Cr�ation d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in

  try
    (* Parsing: la fonction  Parser.prog transforme le tampon lexical en un
       arbre de syntaxe abstraite si aucune erreur (lexicale ou syntaxique)
       n'est d�tect�e.
       La fonction Lexer.token est utilis�e par Parser.prog pour obtenir
       le prochain token. *)
    let p = Parser.prog Lexer.token buf in
    close_in f;

    (* On s'arr�te ici si on ne veut faire que le parsing *)
    if parse_only then exit 0;

    Static_analyze.check_arity p;
    if check_only then exit 0;

    if print then begin Pretty.program p; printf "@." end;

    Sct.prog p 
  with
    | Lexer.Lexing_error c ->
	(* Erreur lexicale. On r�cup�re sa position absolue et
	   on la convertit en num�ro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse lexicale: %s@." c;
	exit 1
    | Parser.Error ->
	(* Erreur syntaxique. On r�cup�re sa position absolue et on la
	   convertit en num�ro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse syntaxique@.";
	exit 1
    | Sct.Error s->
	(* Erreur pendant l'interpr�tation *)
	eprintf "Erreur: %s@." s;
	exit 1
