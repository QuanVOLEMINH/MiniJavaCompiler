{
  open Parser
  open Lexing
}

let letter = ['a'-'z' 'A'-'Z']
let non_zero_digit = ['1'-'9']
let zero_digit = ['0']
let digit = ('0' | non_zero_digit)
let real = digit* ('.' digit*)?
let ident = letter (letter | digit | '_')*
let blank = [' ' '\009']
let integer = digit+
let newline = ('\010' | '\013' | "\013\010")
let onelinecomment = "//" ([^'\010' '\013'])* newline

rule nexttoken = parse
  | newline         { Lexing.new_line lexbuf; nexttoken lexbuf }
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
  | "<="            { print_endline "<"; LTOE }
  | ">="            { print_endline ">"; GTOE }
  | "<<"            { print_endline "<<"; LSHIFT }
  | ">>"            { print_endline ">>"; RSHIFT }
  | ">>>"           { print_endline ">>>"; USHIFT }
    (* separators *)
  | ";"             { print_endline "SEMICOLON"; SEMICOLON }
  | ":"             { print_endline "COLON"; COLON }
  | ","             { print_endline ","; COMMA }
  | "."             { print_endline "."; POINT }
  | "~"             { print_endline "~"; TILDE }
  | "!"             { print_endline "!"; EP }  
  | "?"             { print_endline "?"; QM }
  | "\""             { print_endline "?"; QuoM }    
  | "{"             { print_endline "LPAR"; LPAR }
  | "}"             { print_endline "RPAR"; RPAR }
  | "("             { print_endline "LBRAC"; LBRAC }
  | ")"             { print_endline "RBRAC"; RBRAC }
  | "["             { print_endline "["; LSBRAC }
  | "]"             { print_endline "]"; RSBRAC }
  | "||"            { print_endline "||"; CONDITIONALOR }
  | "&&"            { print_endline "&&"; CONDITIONALAND }
  | "&"             { print_endline "&"; AND }
  | "|"             { print_endline "|"; INCLUSIVEOR }
  | "^"             { print_endline "|"; EXCLUSIVEOR }
  | "=="            { print_endline "=="; CONDITIONALEQUAL }
  | "!="            { print_endline "!="; CONDITIONALNOTEQUAL }
    (* assignment operators *)
  | "="             { print_endline "EQUAL"; EQUAL }
  | "*="            { print_endline "TIMESEQUAL"; TIMESEQUAL }
  | "/="            { print_endline "DIVEQUAL"; DIVEQUAL }
  | "%="            { print_endline "MODEQUAL"; MODEQUAL }
  | "+="            { print_endline "PLUSEQUAL"; PLUSEQUAL }
  | "-="            { print_endline "MINUSEQUAL"; MINUSEQUAL }
  | "<<="           { print_endline "LSHIFTQUAL"; LSHIFTQUAL }
  | ">>="           { print_endline "RSHIFTQUAL"; RSHIFTQUAL }
  | ">>>="          { print_endline "USHIFTQUAL"; USHIFTQUAL }
  | "&="            { print_endline "ANDQUAL"; ANDQUAL }
  | "^="            { print_endline "EXCLUSIVEORQUAL"; EXCLUSIVEORQUAL }
  | "|="            { print_endline "INCLUSIVEORQUAL"; INCLUSIVEORQUAL }
    (* prefix postfix op *)
  | "++"            { print_endline "INCR"; INCR }
  | "--"            { print_endline "DECR"; DECR }
  | "public"        { print_endline "PUBLIC"; PUBLIC }
  | "final"         { print_endline "FINAL"; FINAL } 
  | "void"          { print_endline "VOID"; VOID } 
  | "class"         { print_endline "CLASS"; CLASS }
  | "return"        { print_endline "RETURN"; RETURN}
  | "break"         { print_endline "BREAK"; BREAK}
  | "do"            { print_endline "DO"; DO }
  | "continue"      { print_endline "CONTINUE"; CONTINUE}
  | "while"         { print_endline "WHILE"; WHILE}
  | "if"            { print_endline "IF"; IF}
  | "else"          { print_endline "ELSE"; ELSE}
    (* integral type *)
  | "byte"          { print_endline "BYTE"; BYTE }
  | "short"         { print_endline "SHORT"; SHORT }
  | "int"           { print_endline "INT"; INT }
  | "long"          { print_endline "LONG"; LONG }
  | "char"          { print_endline "CHAR"; CHAR }
  
    (* floating point type *)
  | "float"         { print_endline "FLOAT"; FLOAT }
  | "double"        { print_endline "DOUBLE"; DOUBLE }
  
    (* keywords *)
  | "boolean"       { print_endline "BOOLEAN"; BOOLEAN }
  | "public"        { print_endline "PUBLIC"; PUBLIC }
  | "protected"     { print_endline "PROTECTED"; PROTECTED }
  | "private"       { print_endline "PRIVATE"; PRIVATE }
  | "abstract"      { print_endline "ABSTRACT"; ABSTRACT }
  | "static"        { print_endline "STATIC"; STATIC }
  | "strictfp"      { print_endline "STRICTFP"; STRICTFP }
  | "final"         { print_endline "FINAL"; FINAL } 
  | "void"          { print_endline "VOID"; VOID } 
  | "this"          { print_endline "THIS"; THIS }
  | "super"         { print_endline "SUPER"; SUPER }
  | "extends"       { print_endline "EXTENDS"; EXTENDS }
  | "return"        { print_endline "RETURN"; RETURN}
  | "break"         { print_endline "BREAK"; BREAK}
  | "do"            { print_endline "DO"; DO }
  | "continue"      { print_endline "CONTINUE"; CONTINUE}
  | "while"         { print_endline "WHILE"; WHILE}
  | "class"         { print_endline "CLASS"; CLASS }
    (* literal *)
  | non_zero_digit as nzd { NONZERODIGIT(nzd) }
  | zero_digit { print_endline "0" ; ZERODIGIT }
    (* boolean literal *)
  | "true"          { print_endline "TRUE"; TRUE }
  | "false"         { print_endline "FALSE"; FALSE }
    (* null literal *)
  | "null"          { print_endline "NULL"; NULL}

  | integer as i    { print_endline ("INT: " ^ i ); INTEGER (int_of_string i) }
  | ident as str    { print_endline ("IDENT: " ^ str );IDENT str }

and traditioncommnet = parse (* traditional comment *)
  | "*/"            { nexttoken lexbuf}
  | _               { traditioncommnet lexbuf}
and eolcomment = parse (* end-of-line comment *)
  | newline         { Lexing.new_line lexbuf; nexttoken lexbuf }
  | _               { eolcomment lexbuf }

{
}