{
  type lexeme =
    | EOF
    | PLUS
    | MINUS
    | DIV
    | TIMES
    | FLOAT of float
    | IDENT of string
    | SEMICOLON
    | EQUAL
    | INT of int

  let print_lexeme = function
    | EOF     -> print_string "EOF"
    | PLUS    -> print_string "PLUS"
    | MINUS   -> print_string "MINUS"
    | DIV     -> print_string "DIV"
    | TIMES   -> print_string "TIMES"
    | FLOAT f -> print_string "FLOAT("; print_float f; print_string ")"
    | IDENT s -> print_string "IDENT("; print_string s; print_string ")"
    | SEMICOLON -> print_string "SEMICOLON"
    | EQUAL -> print_string "EQUAL"
    | INT i -> print_string "INT("; print_int i; print_string ")"
  
  open Lexing
  exception Eof
  
  type error = 
    | Illegal_character of char
    | Illegal_float of string
  exception Error of error * position * position

  let raise_error err lexbuf = raise (Error(err, lexeme_start_p lexbuf, lexeme_end_p lexbuf))

  let report_error = function
    | Illegal_character c ->
      print_string "Illegal_character '";
      print_char c;
      print_string "'"
    | Illegal_float nb ->
      print_string "The float ";
      print_string nb;
      print_string " is illegal "
    
  let print_position debut fin = 
    if (debut.pos_lnum = fin.pos_lnum) then
      begin
        print_string "line ";
        print_int debut.pos_lnum;
        print_string " character ";
        print_int (debut.pos_cnum - debut.pos_bol);
        print_string "-";
        print_int (fin.pos_cnum - fin.pos_bol)
      end
    else
      begin
        print_string "from line ";
        print_int debut.pos_lnum;
        print_string " character ";
        print_int (debut.pos_cnum - debut.pos_bol);
        print_string "to line ";
        print_int fin.pos_lnum;
        print_string " character ";
        print_int (fin.pos_cnum - fin.pos_bol)
      end
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
  | _ as c { raise_error (Illegal_character(c)) lexbuf }

{
  let rec examine_all lexbuf =
    let res = nexttoken lexbuf in
    print_lexeme res;
    print_string " ";
    match res with
    | EOF -> ()
    | _   -> examine_all lexbuf
}
