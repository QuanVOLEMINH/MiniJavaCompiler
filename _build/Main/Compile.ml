open Parseexpr
open Lexexpr

(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)
let execute lexbuf verbose =
  try
    let token = Parseexpr.compilationUnit Lexexpr.printtoken lexbuf in
          print_string "=====\n";
          print_string token;
          print_string "=====\n";
  with
    | Parseexpr.Error -> print_string "Not ok";
