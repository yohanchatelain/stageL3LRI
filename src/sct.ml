open Static_analyze
exception Error of string

let print_def p = 
  print_newline();
  List.iter print p;
  print_newline()

let prog p =
  try 
    begin
    check_arity p;
    print_def p;
    (*Printf.printf "Arity is ok"*)
    end  
  with
  |Unknow_arg (t,s) -> 
    Printf.eprintf "\n\nError :\nUnknow %s %s\n\n" t s;
    exit 1
