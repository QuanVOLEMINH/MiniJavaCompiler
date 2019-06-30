let rec inlist e l =
  match l with
  | [] -> false
  | hd::tl -> if hd = e then true else inlist e tl

let sub_list l1 l2 = 
  List.for_all (fun e -> inlist e l2) l1

let intersect_list l1 l2 =
  List.exists (fun e -> inlist e l2) l1

let rec join_list (l: string list) (sign: string) : string =
  match l with
  | [] -> ""
  | hd::[] -> hd
  | hd::tl -> hd^sign^(join_list tl sign)

type valtype  =
  | ValType of Type.t

let is_types_equal (t1: Type.t) (t2: Type.t) =
  ValType(t1)=ValType(t2)

let print_class_name (c: AST.astclass) = 
	print_endline (c.cname)