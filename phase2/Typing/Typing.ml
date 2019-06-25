open TypingHelper

exception Invalid_Inheritance of string
exception Recursive_Inheritance of string

 
let check_modifiers (modifiers: AST.modifier list) = 
  print_endline("To check modifiers")



let rec get_classes (classes: AST.asttype list) = 
    match classes with
    | [] -> []
    | hd::tl -> (
        let cls = get_classes tl in 
        match hd.info with
        | Class cl -> cl.cname <- hd.id; cl::cls
        | Inter -> cls
      )

let rec search_class (c: Type.ref_type) (classesScope: AST.astclass list): AST.astclass =
  print_string ((join_list c.tpath ".")^"."^c.tid^" -> ");
  match classesScope with
  | [] -> raise (Invalid_Inheritance("Class "^(join_list c.tpath ".")^"."^c.tid^" not found."))
  | hd::tl -> (
    match c.tpath with
    | [] -> (
      if hd.cname=c.tid then hd
      else search_class c tl
    )
    | head::tail -> (
      if head="" then (
        search_class { tpath=tail; tid=c.tid } classesScope
      )
      else if hd.cname=head then (
        search_class { tpath=tail; tid=c.tid } hd.cscope
      )
      else search_class c tl
    )
  )

let rec check_classes_dependencies (c: AST.astclass) (id_list: string list) = 
  if inlist c.cid id_list then raise(Recursive_Inheritance("Class "^c.cid^" inherits recursively."))
  else (
    if (List.length c.cparent.tpath == 0 && c.cparent.tid = "Object") then () 
    else (
      let parent = search_class c.cparent c.cscope in
			check_classes_dependencies parent (c.cid::id_list) 
    )
  ) 

let rec check_classes_dependencies_iter (c: AST.astclass)=
  check_classes_dependencies c [];
  List.map check_classes_dependencies_iter (get_classes c.ctypes);
  ()

let rec check_classes (classes: AST.astclass list) =
  List.map check_classes_dependencies_iter classes 
    
let rec check_inner_classes (c: AST.astclass) (classesScope: AST.astclass list) (id_list: string list) = 
  []
  
let rec add_package (packageName: AST.qualified_name) (classes: AST.astclass list) (id: string) = 
  match packageName with
  | [] -> []
  | hd::[] -> 
      [{
        AST.cid = id^hd;
        AST.cname = hd;
        AST.cscope = classes;
        AST.cmodifiers = [];
        AST.cparent = { tpath=[]; tid="Object" };
        AST.cattributes = [];
        AST.cinits = [];
        AST.cconsts = [];
        AST.cmethods = [];
        AST.ctypes = [];
        AST.cloc = Location.none;
      }]
  | hd::tl ->
      let next = add_package tl classes (id^hd^".") in
      [{
        AST.cid=id^hd;
        AST.cname=hd;
        AST.cscope=next;
        AST.cmodifiers=[];
        AST.cparent = { tpath=[] ; tid="Object" } ;
        AST.cattributes = [];
        AST.cinits = [];
        AST.cconsts = [];
        AST.cmethods = [];
        AST.ctypes = [];
        AST.cloc = Location.none;
      }]

let get_package (packageName: AST.qualified_name option) (classes: AST.astclass list): AST.astclass list =
  match packageName with
  | None -> classes
  | Some pn -> (add_package pn classes "")@classes

let get_package_info (ast : AST.t): string = 
  match ast.package with 
    | None -> ""
    | Some packageNameList -> join_list packageNameList "."

let get_program_info (packageName: AST.qualified_name option) (classes: AST.astclass list): AST.astclass list =
  let classesList = get_package packageName classes in

	let objectClass = {		
		  AST.cid="Object";	
      AST.cname="Object";	
      AST.cscope = [];
      AST.cmodifiers = [AST.Public];
      AST.cparent = { tpath=[]; tid="Object" } ;
      AST.cattributes = [
            {
                amodifiers = [AST.Static];
                aname = "class";
                atype = Type.Ref Type.class_type;
                adefault = None;
                aloc = Location.none;
            }
        ];
      AST.cinits = [];
      AST.cconsts = [];
      AST.cmethods = [];
      AST.ctypes = [];	
      AST.cloc = Location.none;
    } in 
  objectClass.cscope<-[objectClass];
  objectClass::classesList




(* starting point *)
let type_program (program: AST.t) = 
  let classes =  get_program_info (program.package) (get_classes program.type_list) in
  List.iter (fun c -> AST.print_class "*--*" c) classes;
  check_classes classes;
  program


