{
  open Parseexpr
}


let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let space = [' ' '\t' '\n']

rule nexttoken = parse
  | space+        { nexttoken lexbuf }
  | eof           { EOF }
  | "+"           { PLUS } 
  | "-"           { MINUS } 
  | "/"           { DIV } 
  | "*"           { TIMES } 
  | "%"           { MOD } 
  | digit+ as nb  { INT (int_of_string nb) }
  | ident         { IDENT (Lexing.lexeme lexbuf) }