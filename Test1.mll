{
  type lexeme =
    | EOF
    | PLUS
    | MINUS
    | DIV
    | TIMES
    | FLOAT of float
    | IDENT of string
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let real = digit* ('.' digit*)?
let ident = letter (letter | digit | '_')*
let space = [' ' '\t' '\n']

rule nexttoken = parse
  | space+        { nexttoken lexbuf }
  | eof           { EOF }
  | "+"           { PLUS } 
  | "-"           { MINUS } 
  | "/"           { DIV } 
  | "*"           { TIMES } 
  | real as nb    { FLOAT (float_of_string nb) }
  | ident as str  { IDENT str }
