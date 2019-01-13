{
  open Parser
  open Lexing
}

let letter = ['a'-'z' 'A'-'Z']
let non_zero_digit = ['1'-'9']
let digit = ('0' | non_zero_digit)
let real = digit* ('.' digit*)?
let ident = letter (letter | digit | '_')*
let blank = [' ' '\009']
let integer = digit+
let newline = ('\010' | '\013' | "\013\010")
let onelinecomment = "//" ([^'\010' '\013'])* newline

rule nexttoken = parse
  | onelinecomment { Lexing.new_line lexbuf; nexttoken lexbuf }
  | newline { Lexing.new_line lexbuf; nexttoken lexbuf }
  | blank+ { nexttoken lexbuf }
  | eof { EOF }
  | "+" { PLUS } 
  | "-" { MINUS } 
  | "*" { TIMES }
  | "/" { DIV } 
  | ";" { SEMICOLON }
  | "{" { LPAR }
  | "}" { RPAR }
  | "(" { LBRAC }
  | ")" { RBRAC }
  | "=" { EQUAL }
  | "++" { INCR }
  | "public" { PUBLIC } 
  | "class" { CLASS }
  | "int"  { INTEGER }
  | integer as i { INT (int_of_string i) }
  | ident as str  { IDENT str }

{
let print_lexeme = function
    | EOF -> print_string "EOF"
    | PLUS -> print_string "+"
    | MINUS -> print_string "-"
    | TIMES -> print_string "*"
    | DIV -> print_string "/"
    | SEMICOLON -> print_string ";"
    | LPAR -> print_string "{"
    | RPAR -> print_string "}"
    | LBRAC -> print_string "("
    | RBRAC -> print_string ")"
    | EQUAL -> print_string "="
    | INCR -> print_string "++"
    | INT i -> print_string "INT("; print_string (string_of_int i); print_string ")"
    | IDENT s -> print_string "IDENT("; print_string s; print_string ")"
    | PUBLIC -> print_string "public"
    | INTEGER -> print_string "int"
    | CLASS -> print_string "class"

let rec printtoken buf = 
  let token = nexttoken buf in
      print_int buf.lex_curr_p.pos_lnum;
      print_string ".     ";
      print_lexeme token;
      print_string "\n";
      token
}