type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod

type unopleft =
  | Uplus | Uminus

type assignop = 
  | Equal

type expression =
  | Const of int
  | Var of string
  | Binop of expression * binop * expression
  | Unopleft of unopleft * expression
  | Assign of expression * assignop * expression
  (* | Name of expression list *)

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

let string_of_assignop = function
  | Equal -> "="

let rec string_of_expr exp =
  match exp with
  | Const c -> string_of_int c
  | Var v -> v
  | Binop(e1, op, e2) -> 
      "(" ^(string_of_expr e1)^ (string_of_op_b op) ^(string_of_expr e2)^ ")"
  | Unopleft(op, e) -> "(" ^ (string_of_op_uleft op) ^(string_of_expr e)^ ")"
  | Assign(e1, op, e2) -> "(" ^(string_of_expr e1)^ (string_of_assignop op) ^(string_of_expr e2)^ ")"
  (* | Name(l) -> (string_of_list ", " string_of_expr l) *)
