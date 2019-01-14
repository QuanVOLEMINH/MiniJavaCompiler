
(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)
let execute lexbuf verbose =
  try
    let token = Parser.compilationUnit Lexer.printtoken lexbuf in
      print_string "\n====== Parser print =====\n\n";
      print_string token;
      print_string "\n\n=====\n"
  with
    | Parser.Error -> print_string "Parser error\n";
