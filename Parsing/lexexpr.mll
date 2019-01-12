{
  open Parseexpr

  exception SyntaxError 
}


let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009']

rule nexttoken = parse
  | newline       { Lexing.new_line lexbuf; nexttoken lexbuf }
  | blank+        { nexttoken lexbuf }
  | "/*"          { traditioncommnet lexbuf }
  | "//"          { eolcomment lexbuf }
  | ";"           { SEMICOLON}
  | "+"           { PLUS } 
  | "-"           { MINUS } 
  | "/"           { DIV } 
  | "*"           { TIMES } 
  | "%"           { MOD } 
  | digit+ as nb  { INT (int_of_string nb) }
  | ident         { IDENT (Lexing.lexeme lexbuf) }
  | _             { raise (SyntaxError)}
  | eof           { EOF }
and traditioncommnet = parse (* traditional comment *)
  | "*/"          { nexttoken lexbuf}
  | _             { traditioncommnet lexbuf}
and eolcomment = parse (* end-of-line comment *)
  | newline       { Lexing.new_line lexbuf; nexttoken lexbuf }
  | _             { eolcomment lexbuf }