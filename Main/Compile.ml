open Parser 
open Lexer 
open Expr
(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)
let execute lexbuf verbose =
  try
    let token = Parser.compilationUnit nexttoken lexbuf in
      print_string "Parser print=====\n";
      print_string token;
      print_string "\n\n=====\n"
  with
    | Parser.Error -> print_string "Parser error\n";
