{
  type lexeme =
    | EOF
    | FLOAT of float
    | IDENT of string

  let print_lexeme = function
    | EOF     -> print_string "EOF"
    | FLOAT f -> print_string "FLOAT("; print_float f; print_string ")"
    | IDENT s -> print_string "IDENT("; print_string s; print_string ")"
}


let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let real = digit* ('.' digit*)?
let ident = letter (letter | digit | '_')*
let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009']

rule nexttoken = parse
  | newline       { Lexing.new_line lexbuf; nexttoken lexbuf }
  | blank+        { nexttoken lexbuf }
  | "/*"          { traditioncommnet lexbuf }
  | "//"          { eolcomment lexbuf }
  | eof           { EOF }
  | real as nb    { FLOAT (float_of_string nb) }
  | ident as str  { IDENT str }
and traditioncommnet = parse
  | "*/"          { nexttoken lexbuf}
  | _             { traditioncommnet lexbuf}
and eolcomment = parse
  | newline       { Lexing.new_line lexbuf; nexttoken lexbuf }
  | _             { eolcomment lexbuf }


{
  let rec examine_all lexbuf =
    let res = nexttoken lexbuf in
    print_lexeme res;
    print_string " ";
    match res with
    | EOF -> ()
    | _   -> examine_all lexbuf
        
  let compile file =
  print_string ("File "^file^" is being treated!\n");
  let input_file = open_in file in
  let lexbuf = Lexing.from_channel input_file in
  examine_all lexbuf;
  print_newline ();
  close_in (input_file)
  let _ = Arg.parse [] compile ""
}
