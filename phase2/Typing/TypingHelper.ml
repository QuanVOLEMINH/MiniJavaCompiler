let rec inlist e l =
  match l with
  | [] -> false
  | hd::tl -> if hd = e then true else inlist e tl


let rec join_list (l: string list) (sign: string) : string =
  match l with
  | [] -> ""
  | hd::[] -> hd
  | hd::tl -> hd^sign^(join_list tl sign)


let print_class_name (c: AST.astclass) = 
	print_endline (c.cname)