open TypingHelper

exception Invalid_Inheritance of string
exception Recursive_Inheritance of string

 
let check_modifiers (modifiers: AST.modifier list) = 
  print_endline("To check modifiers")

let rec check_classes func classes classesScope =
  match classes with
  | [] -> ()
  | hd::tl -> func hd classesScope []; check_classes func tl classesScope 

let rec get_classes (classes: AST.asttype list) = 
    match classes with
    | [] -> ([], [])
    | hd::tl -> (
        check_modifiers hd.modifiers;
        let classes, interfaces = get_classes tl in 
        match hd.info with
        | Class cl -> cl.cid <- hd.id; (cl::classes, interfaces)
        | Inter -> (classes, hd::interfaces)
      )

let rec search_class (c: Type.ref_type) (classesScope: AST.astclass list): AST.astclass = 
  match classesScope with
  | [] -> raise (Invalid_Inheritance("Class "^(join_list c.tpath ".")^"."^c.tid^" not found."))
  | hd::tl -> (
    match c.tpath with
    | [] -> (
      if hd.cid=c.tid then hd
      else search_class c tl
    )
    | elem::rest -> (
      if hd.cid=elem then (
        let classes, interfaces = get_classes hd.ctypes in
        search_class { tpath=rest; tid=c.tid } classes
      )
      else search_class c tl
    )
  )

let rec check_classes_dependencies (c: AST.astclass) (classesScope: AST.astclass list) (id_list: string list) = 
  if inlist c.cid id_list then raise(Recursive_Inheritance("Class "^c.cid^" inherits recursively."))
  else 
    if c.cparent.tid = "Object" then () 
    else check_classes_dependencies (search_class c.cparent classesScope) classesScope (c.cid::id_list)

let rec check_inner_classes (c: AST.astclass) (classesScope: AST.astclass list) (id_list: string list) = 
  []
  
let rec add_package (package_name: AST.qualified_name) (list_classes: AST.astclass list) (id: string) = 
  match package_name with
  | [] -> []
  | tl::[] -> 
      [{
        AST.cid = id^tl;
        AST.cname = tl;
        AST.cscope = list_classes;
        AST.cmodifiers = [];
        AST.cparent = { tpath = []; tid="Object" };
        AST.cattributes = [];
        AST.cinits = [];
        AST.cconsts = [];
        AST.cmethods = [];
        AST.ctypes = [];
        AST.cloc = Location.none;
      }]
  | hd::tl ->
      let next = add_package tl list_classes (id^hd^".") in
      [{
        AST.cid=id^hd;
        AST.cname=hd;
        AST.cscope=next;
        AST.cmodifiers=[];
        AST.cparent = {tpath=[];tid="Object"} ;
        AST.cattributes = [];
        AST.cinits = [];
        AST.cconsts = [];
        AST.cmethods = [];
        AST.ctypes = [];
        AST.cloc = Location.none;
      }]

let get_package (package_name: AST.qualified_name option) (list_classes: AST.astclass list): AST.astclass list =
  match package_name with
  | None -> list_classes
  | Some pn -> (add_package pn list_classes "")@list_classes


let get_program_info (package_name: AST.qualified_name option) (list_classes: AST.astclass list): AST.astclass list =
  let classes = get_package package_name list_classes in
  List.iter (fun c -> AST.print_class "*--*" c) classes;
  classes


let type_program (program: AST.t) = 
  (* let classes = get_program_info (program.package) (get_classes program.type_list) in
  List.iter (fun t -> AST.print_type "===" t) program.type_list;
  program *)
  let classes, interfaces = get_classes program.type_list in
  (* List.iter (fun c -> AST.print_type "*--*" c) classes; *)
  check_classes check_classes_dependencies classes classes;
  check_classes check_inner_classes classes classes
