exception Invalid_Inheritance of string

let rec inlist e l =
  match l with
  | [] -> false
  | hd::tl -> if hd = e then true else inlist e tl

let rec searchClass id (classesScope: AST.asttype list) = 
  match classesScope with
  | [] -> raise (Invalid_Inheritance("Class "^id^" not found."))
  | hd::tl -> (
    if hd.id = id then hd
    else searchClass id tl
  )
 
let checkModifiers (modifiers: AST.modifier list) = 
  print_endline("To check modifiers")

let rec checkClassesDependencies (c: AST.asttype) (classesScope: AST.asttype list) id_list = 
  if inlist c.id id_list then print_endline("to check recursive_inherit")
  else 
    match c.info with 
    | Class cl -> ( 
        if cl.cparent.tid = "Object" then () 
        else checkClassesDependencies (searchClass cl.cparent.tid classesScope) classesScope (c.id::id_list)
      )
    | Inter -> ()

let rec checkClasses classes classesScope =
  match classes with
  | [] -> ()
  | hd::tl -> checkClassesDependencies hd classesScope []; checkClasses tl classesScope 

let rec get_classes (classes: AST.asttype list) = 
    match classes with
    | [] -> ([], [])
    | hd::tl -> (
        checkModifiers hd.modifiers;
        let c, i = get_classes tl in 
        match hd.info with
        | Class cl -> (hd::c, i)
        | Inter -> (c, hd::i)
      )

  (* package_name is a list of strings *)
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
  List.iter (fun c -> AST.print_type "*--*" c) classes;
  checkClasses classes classes