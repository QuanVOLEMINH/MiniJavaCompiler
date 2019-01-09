open Parseexpr
open Lexexpr
open Expr

(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)
let execute lexbuf verbose = 
  print_endline "parsing todo";
  let exp = expression nexttoken lexbuf in 
  print_string (string_of_expr exp);
