type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod

type unopleft =
  | Uplus | Uminus
type assign = 
  | Equal

type expression =
  | Const of int
  | Var of string
  | Binop of expression * binop * expression
  | Unopleft of unopleft * expression
  | Assign of expression * assign * expression

exception Unbound_variable of string

let string_of_op_uleft = function
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
  | Binop(e1, op, e2) -> 
      "(" ^(string_of_expr e1)^ (string_of_op_b op) ^(string_of_expr e2)^ ")"
  | Unopleft(op, e) -> "(" ^ (string_of_op_uleft op) ^(string_of_expr e)^ ")"
