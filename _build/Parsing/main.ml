open Parseexpr
open Lexexpr
open Expr

let compile file =
  print_string ("File "^file^" is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    let exp = expression nexttoken lexbuf in
    print_string (string_of_expr exp);
    print_string " = ";
    print_int (eval [] exp);
    print_newline();
    close_in (input_file);
    exit 0
  with Sys_error s ->
    print_endline ("Can't find file '" ^ file ^ "'")
      
let _ = Arg.parse [] compile ""