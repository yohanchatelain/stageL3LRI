exception Error of string

let prog p =
  let call = Substitution.call p in
  List.iter Pretty.call call
