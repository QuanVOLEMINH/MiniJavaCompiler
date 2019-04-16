open Parser

let execute lexbuf verbose = 
  try 
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "successfull parsing";
    if verbose then AST.print_program ast;
    Typing.typage ast;
    print_endline "FIN DE TYPAGE"
  with 
    | Typing.Class_exist class_name  -> print_endline ("Class "^class_name^" already exist")
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
