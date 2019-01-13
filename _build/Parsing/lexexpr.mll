{
  open Parseexpr
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let real = digit* ('.' digit*)?
let ident = letter (letter | digit | '_')*
let blank = [' ' '\009']
let int = digit+
let newline = ('\010' | '\013' | "\013\010")
let onelinecomment = "//" ([^'\010' '\013'])* newline

rule nexttoken = parse
  | newline { Lexing.new_line lexbuf; nexttoken lexbuf }
  | blank+ { nexttoken lexbuf }
  | eof { EOF }
  | "+" { PLUS } 
  | "-" { MINUS } 
  | "*" { TIMES }
  | "/" { DIV } 
  | ";" { SEMICOLON }
  | "=" { EQUAL }
  | ident as str  { IDENT str }
  | int as nb  { INT (int_of_string nb) }
  | onelinecomment { Lexing.new_line lexbuf; nexttoken lexbuf }

{
let print_lexeme = function
    | EOF -> print_string "EOF"
    | PLUS -> print_string "+"
    | MINUS -> print_string "-"
    | TIMES -> print_string "*"
    | DIV -> print_string "/"
    | SEMICOLON -> print_string ";"
    | EQUAL -> print_string "="
    | INT i -> print_string "INT("; print_string(string_of_int i); print_string ")"
    | IDENT s -> print_string "IDENT("; print_string s; print_string ")"

let rec printtoken buf = 
  let token = nexttoken buf in
      print_int buf.lex_curr_p.pos_lnum;
      print_string ".     ";
      print_lexeme token;
      print_string "\n";
      token
}