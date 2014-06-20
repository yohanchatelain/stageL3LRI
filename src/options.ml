let b = ref 1
let d = ref 2
let parse_only = ref false
let check_only = ref false
let print = ref true
let print_step = ref false
let source_file = ref ""

let set_file s = source_file := s
let usage = "usage: sct [option] file.sct"

let options =
  ["--parse-only", Arg.Set parse_only,
   "  Pour ne faire uniquement que la phase d'analyse syntaxique";
   "--check-only", Arg.Set check_only,
   "  Pour ne faire uniquement que la vérification d'arité";
   "--no-print", Arg.Clear print,
   "  N'affiche pas l'arbre de syntax abstraite";
   "--print-step", Arg.Set print_step,
   "  Affiche la cloture des graphes de chemin pas à pas";
   "-B", Arg.Set_int b,
   "  Spécifier le poids (strictement positif)";
   "-D", Arg.Set_int d,
   "  Spécifier la profondeur (positive ou nulle)"
  ]
 
let () = 
  Arg.parse options set_file usage

let b = !b
let d = !d
let parse_only = !parse_only
let check_only = !check_only
let print = !print
let print_step = !print_step
let source_file = !source_file
  
