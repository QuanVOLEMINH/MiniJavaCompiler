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
  | newline         { print_endline "newline"; Lexing.new_line lexbuf; nexttoken lexbuf }
  | blank+          { nexttoken lexbuf }
  | "/*"            { print_endline "traditioncommnet"; traditioncommnet lexbuf }
  | "//"            { print_endline "eolcomment"; eolcomment lexbuf }
  | eof             { print_endline "EOF"; EOF }
  | "+"             { print_endline "PLUS"; PLUS } 
  | "-"             { print_endline "MINUS"; MINUS } 
  | "*"             { print_endline "TIMES"; TIMES }
  | "/"             { print_endline "DIV"; DIV }
  | "<"             { print_endline "<"; LT }
  | ">"             { print_endline ">"; GT }
  | ";"             { print_endline "SEMICOLON"; SEMICOLON }
  | ","             { print_endline ","; COMMA }
  | "{"             { print_endline "LPAR"; LPAR }
  | "}"             { print_endline "RPAR"; RPAR }
  | "("             { print_endline "LBRAC"; LBRAC }
  | ")"             { print_endline "RBRAC"; RBRAC }
  | "["             { print_endline "["; LSBRAC }
  | "]"             { print_endline "]"; RSBRAC }
  | "="             { print_endline "EQUAL"; EQUAL }
  | "++"            { print_endline "INCR"; INCR }
  | "public"        { print_endline "PUBLIC"; PUBLIC }
  | "final"        { print_endline "FINAL"; FINAL } 
  | "class"         { print_endline "CLASS"; CLASS }
  | "int"           { print_endline "INTEGER"; INTEGER }
  | integer as i    { INT (int_of_string i) }
  | ident as str    { IDENT str }
and traditioncommnet = parse (* traditional comment *)
  | "*/"            { nexttoken lexbuf}
  | _               { traditioncommnet lexbuf}
and eolcomment = parse (* end-of-line comment *)
  | newline         { Lexing.new_line lexbuf; nexttoken lexbuf }
  | _               { eolcomment lexbuf }

{
let print_lexeme = function
    | EOF -> print_string "EOF"
    | PLUS -> print_string "+"
    | MINUS -> print_string "-"
    | TIMES -> print_string "*"
    | DIV -> print_string "/"
    | LT -> print_string "<"
    | GT -> print_string ">"
    | SEMICOLON -> print_string ";"
    | COMMA -> print_string ","
    | LPAR -> print_string "{"
    | RPAR -> print_string "}"
    | LBRAC -> print_string "("
    | RBRAC -> print_string ")"
    | LSBRAC -> print_string "["
    | RSBRAC -> print_string "]"
    | EQUAL -> print_string "="
    | INCR -> print_string "++"
    | INT i -> print_string "INT("; print_string (string_of_int i); print_string ")"
    | IDENT s -> print_string "IDENT("; print_string s; print_string ")"
    | PUBLIC -> print_string "public"
    | FINAL -> print_string "final"
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