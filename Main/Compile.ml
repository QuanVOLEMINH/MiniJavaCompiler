open Parseexpr
open Lexexpr
open Expr
open Location


let parse_with_error lexbuf =
  try expression nexttoken lexbuf with 
    | SyntaxError ->
      print_endline "Error in lexical analyse step";
      print (curr lexbuf);
      None

let rec parse_and_print lexbuf = 
    match parse_with_error lexbuf with
    | Some expr ->
      print_string (string_of_expr expr) ;
      parse_and_print lexbuf
    | None -> ()

(* verbose is a boolean that you can use to switch to a verbose output (for example, to dump all the ast) *)
let execute lexbuf verbose = 
  print_endline "parsing todo";
  parse_and_print lexbuf
  