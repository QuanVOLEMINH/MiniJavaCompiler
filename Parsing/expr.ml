type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod

type unop =
  | Uplus | Uminus

type expression =
  | Const of int
  | Var of string
  | Binop of binop * expression * expression
  | Unop of unop * expression

exception Unbound_variable of string

let string_of_op_u = function
  | Uplus -> "+"
  | Uminus -> "-"

let string_of_op_b = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "/"
  | Bmod -> "%"

let rec string_of_expr exp =
  match exp with
  | Const c -> string_of_int c
  | Var v -> v
  | Binop(op, e1, e2) -> 
      "(" ^(string_of_expr e1)^ (string_of_op_b op) ^(string_of_expr e2)^ ")"
  | Unop(op, e) -> "(" ^ (string_of_op_u op) ^(string_of_expr e)^ ")"

