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

rule nexttoken = parse
  | newline { Lexing.new_line lexbuf; nexttoken lexbuf }
  | blank+        { nexttoken lexbuf }
  | eof           { EOF }
  | "+"           { PLUS } 
  | "-"           { MINUS } 
  | "/"           { DIV } 
  | "*"           { TIMES }
  | ";"  	        { SEMICOLON }
  | "="  	        { EQUAL }
  | int as nb  { INT (int_of_string nb) }
  | real as nb    { FLOAT (float_of_string nb) }
  | ident as str  { IDENT str }
