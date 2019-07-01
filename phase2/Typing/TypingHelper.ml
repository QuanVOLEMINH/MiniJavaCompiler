let rec inlist e l =
  match l with
  | [] -> false
  | hd::tl -> if hd = e then true else inlist e tl

let sub_list l1 l2 = 
  List.for_all (fun e -> inlist e l2) l1

let intersect_list l1 l2 =
  List.exists (fun e -> inlist e l2) l1

let rec get_last (l: string list) : (string list) * string=
  match l with
  | [] -> ([], "")
  | hd::[] -> ([], hd)
  | hd::tl -> (
    let (h, t) = get_last tl in 
    (hd::h, t)
  )

let rec join_list (l: string list) (sign: string) : string =
  match l with
  | [] -> ""
  | hd::[] -> hd
  | hd::tl -> hd^sign^(join_list tl sign)

let rec split (str: string) (c: char) (strSize:int) (currentPos:int) (current:string) : string list=
  if currentPos = strSize then
    if current="" then
      []
    else
      current::[]
  else
    if str.[currentPos] = c then
      current::(split str c strSize (currentPos+1) "")
    else 
      split str c strSize (currentPos+1) (current^(String.make 1 str.[currentPos]))

let string_split (str: string) (c: char) : string list =
  split str c (String.length str) 0 ""

let get_path (path: string): string list =
  let (first, last) = get_last (string_split path '.' ) in 
  first
  
type valtype  =
  | ValType of Type.t

let is_types_equal (t1: Type.t) (t2: Type.t) =
  ValType(t1)=ValType(t2)

let print_class_name (c: AST.astclass) = 
	print_endline (c.cname)