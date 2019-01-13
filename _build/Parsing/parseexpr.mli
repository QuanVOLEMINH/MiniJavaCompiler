
(* The type of tokens. *)

type token = 
  | TIMES
  | SEMICOLON
  | RPAR
  | PLUS
  | MOD
  | MINUS
  | LPAR
  | INT of (int)
  | IDENT of (string)
  | EQUAL
  | EOF
  | DIV

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val compilationUnit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (string)
