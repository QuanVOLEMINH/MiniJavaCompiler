open TypingHelper
open ExceptionsDef

let check_modifiers (modifiers: AST.modifier list) = 
  print_endline("To check modifiers")



let rec get_classes (innerClasses: AST.asttype list) = 
    match innerClasses with
    | [] -> []
    | hd::tl -> (
        let cls = get_classes tl in 
        match hd.info with
        | Class cl -> cl.cname <- hd.id; cl.cmodifiers <- hd.modifiers; cl::cls
        | Inter -> cls
      )

let rec search_class (c: Type.ref_type) (classesScope: AST.astclass list): AST.astclass =
  (* print_endline ((join_list c.tpath ".")^"."^c.tid^" -> "); *)
  match classesScope with
  | [] -> raise (InvalidInheritance("Class "^(join_list c.tpath ".")^"."^c.tid^" not found."))
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

let rec get_scopes (cid: string) (classes: AST.astclass list) (classesScope:AST.astclass list)  = 
  let fill_scope = add_scope cid classesScope in
  List.map fill_scope classes
and add_scope (cid: string) (classesScope: AST.astclass list) (cl: AST.astclass) =
  print_endline ("add scope: "^cl.cid^"--"^cl.cname);
  if(cl.cid="") then (
    let cls = get_classes cl.ctypes in
    print_endline("--- Inner class of class "^cl.cname);
    List.map print_class_name cls;
    print_endline("---");
    print_endline("--- Types of class "^cl.cname);
    List.map (AST.print_type ("   ")) cl.ctypes;
    print_endline("---");
    cl.cid<-cid^"."^cl.cname;
    cl.cscope<-(cls@classesScope);
    get_scopes cl.cid cls (cls@classesScope);
    cl
  )
  else cl

(* class dependencies: inheritance *)
let rec check_cls_dependencies (c: AST.astclass) (id_list: string list) = 
  if inlist c.cid id_list then raise(RecursiveInheritance("Class "^c.cid^" inherits recursively."))
  else (
    if (List.length c.cparent.tpath == 0 && c.cparent.tid = "Object") then () 
    else (
      let parent = search_class c.cparent c.cscope in
			check_cls_dependencies parent (c.cid::id_list) 
    )
  ) 

let rec check_class_dependencies (c: AST.astclass)=
  check_cls_dependencies c [];
  List.map check_class_dependencies (get_classes c.ctypes);
  ()
(**)

(* class methods *)
let check_dup_methods (methods: AST.astmethod list) =
  print_endline("Duplicate methods def")

let check_method_modifiers (modifiers: AST.modifier list) = 
  print_endline "method modifiers"

let check_method_dup_args (args: AST.argument list) = 
  print_endline "method dup args"

let check_method_body (cl: AST.astclass) (mBody: AST.statement list) = 
  print_endline "method body"

let check_class_method (cl: AST.astclass) (mt: AST.astmethod) = 
  print_endline("class method");
  check_method_modifiers mt.mmodifiers;
  check_method_dup_args mt.margstype;
  check_method_body cl mt.mbody


let rec check_class_methods (cl: AST.astclass) = 
  check_dup_methods cl.cmethods;
  List.map (check_class_method cl) cl.cmethods;
  List.map check_class_methods (get_classes cl.ctypes);
  ()
(**)

(* duplicate class *)
let rec check_dup_class_name (classNameList: string list) =
  match classNameList with
  | [] -> ()
  | hd::tl -> (
    if (inlist hd tl) then raise(DuplicateClass("Duplicate class definition: "^hd));
    check_dup_class_name tl;
    ()
  )

let rec check_dup_class (classes: AST.asttype list) =
  let classeNameList = List.map (fun (x: AST.asttype) -> x.id;) classes in 
  check_dup_class_name classeNameList;
	List.map (fun (c: AST.asttype) -> 
				match c.info with
				 | Class cl -> check_dup_class (cl.ctypes)
				 | Inter -> ()
			) classes;
	()
(**)

(* class modifier *)
let rec check_dup_modifiers (modifiers: AST.modifier list) = 
  match modifiers with
  | [] -> ()
  | hd::tl -> (
    if inlist hd tl then raise (DuplicateModifier("Duplicate modifier: "^(AST.stringOf_modifier hd)));
    check_dup_modifiers tl
  )

let rec check_multi_access_modifiers (modifiers: AST.modifier list) =
  let res = List.filter (fun x -> (inlist x Modifiers.accessModifiers)) modifiers in
  if List.length res > 1 then raise (MultiAccessModifiers ("Invalid multiple modifiers: "^(join_list (List.map AST.stringOf_modifier res) " & ")))
  
let check_modifiers (modifiers: AST.modifier list) = 
  (* List.map (fun m -> (print_endline (AST.stringOf_modifier m))) modifiers; *)
  check_dup_modifiers modifiers;
  check_multi_access_modifiers modifiers

let rec check_class_modifiers (cl: AST.astclass) = 
  check_modifiers cl.cmodifiers;
  
  if not (List.for_all (fun m -> inlist m Modifiers.allModifiers;) cl.cmodifiers) then raise (InvalidModifier ("Invalid modifier at class: "^cl.cname));
  
  let ims = Modifiers.illegalPresentModifiers in
  if (sub_list ims cl.cmodifiers) then raise (IllegalModifierCombination ("Illegal modifier combinations: "^(join_list (List.map AST.stringOf_modifier ims) " & ")));
  
  List.map check_class_modifiers (get_classes cl.ctypes);
  ()
(**)

(* class attributes *)
let check_class_dup_attrs (attrs: AST.astattribute list) = 
	print_endline "class dup attrs"

let check_class_attr_modifiers (attr: AST.astattribute) = 
  print_endline "class attr mod"

let check_class_attr_coherence (attr: AST.astattribute) = 
  print_endline "attr coherence"

let rec check_class_attributes (cl: AST.astclass) = 
  print_endline("attr def");
  check_class_dup_attrs cl.cattributes;
  List.map check_class_attr_modifiers cl.cattributes;
  List.map check_class_attr_coherence cl.cattributes;
  List.map check_class_attributes (get_classes cl.ctypes);
  ()
(**)

(* class constructors *)
let rec check_class_constructors (cl: AST.astclass) = 
  print_endline("constructors def");
  ()
(**)

(* class initialization *)
let rec check_class_initializations (cl: AST.astclass) = 
  print_endline("initialization def");
  ()
(**)

let rec check_classes (prog: AST.t) (classes: AST.astclass list) =
  check_dup_class prog.type_list;
  List.map check_class_modifiers classes;
  List.map check_class_dependencies classes;
  List.map check_class_attributes classes;
  List.map check_class_methods classes;
  List.map check_class_constructors classes;
  List.map check_class_initializations classes

    
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
        AST.cparent = {tpath=[]; tid="Object"};
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
        AST.cparent = {tpath=[] ; tid="Object"};
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
  Object.objectClass::classesList




(* starting point *)
let type_program (program: AST.t) = 
  let classes =  get_program_info (program.package) (get_classes program.type_list) in
  (* List.iter (fun c -> print_class_name c) classes; *)
  get_scopes (get_package_info program) classes classes;
  check_classes program classes;
  program


