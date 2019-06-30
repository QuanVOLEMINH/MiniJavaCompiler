open TypingHelper
open ExceptionsDef


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

let rec is_child_class (cScope: AST.astclass list) (child: Type.t) (parent: Type.t) =
  if (ValType(child)=ValType(parent)) then true
  else
    match child with 
    | Ref r -> (
      if (r = Type.object_type) then false
      else (
        let cl = search_class r cScope in
        is_child_class cl.cscope (Ref cl.cparent) parent
      )
    )
    | Array (ct, ci) -> (
        match parent with
        | Array(pt, pi) -> (
          if ((pi=ci) && (is_child_class cScope ct pt)) then true
          else 
          (
            match ct with 
            | Ref cr -> (
              match pt with 
              | Ref pr -> (
                if((List.length pr.tpath)=0 && (List.length cr.tpath)=0 && (cr.tid=pr.tid)) then true else false
              )
              | _ -> false
            )
            |_ ->	false
          )
        )
        | _ -> false     
    )
    | _ -> false
  
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

(* class modifier *)
let rec check_dup_modifiers (modifiers: AST.modifier list) = 
  match modifiers with
  | [] -> ()
  | hd::tl -> (
    if inlist hd tl then raise (DuplicateModifier("Duplicate modifier: "^(AST.stringOf_modifier hd)));
    check_dup_modifiers tl;
    ()
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
  
  if not (List.for_all (fun m -> inlist m Modifiers.classModifiers;) cl.cmodifiers) then raise (InvalidModifier ("Invalid modifier at class: "^cl.cname));
  
  let imcFA = Modifiers.illegalModifierCombiFA in
  if (sub_list imcFA cl.cmodifiers) then raise (IllegalModifierCombination ("Illegal modifier combinations: "^(join_list (List.map AST.stringOf_modifier imcFA) " & ")));
  
  List.map check_class_modifiers (get_classes cl.ctypes);
  ()
(**)

(* class methods *)
let check_method_in_list (methods: AST.astmethod list) (mt: AST.astmethod) =
  List.iter (
		fun (m: AST.astmethod) -> if m.mname=mt.mname then
		(
			if (List.length mt.margstype)=(List.length m.margstype) then
			(
				let equalTypes = List.map2 (fun (a1: AST.argument) (a2: AST.argument) -> TypingHelper.is_types_equal a1.ptype a2.ptype;) mt.margstype m.margstype in
        
        if (List.for_all (fun x -> x) equalTypes) then raise (DuplicateMethod ("Duplicate Method Definition: "^m.mname))
			)
		);
  ) methods

let rec check_dup_methods (methods: AST.astmethod list) =
  match methods with
	| [] -> ()
	| hd::tl -> (
    check_method_in_list tl hd; 
    check_dup_methods tl;
    ()
  )

let check_method_modifiers (modifiers: AST.modifier list) (cl: AST.astclass) (mt: AST.astmethod) = 
  check_modifiers modifiers;
  
  if not (List.for_all (fun m -> inlist m Modifiers.methodModifiers;) modifiers) then raise (InvalidModifier ("Invalid modifier at class: "^cl.cname^", method: "^mt.mname));

  if ((inlist AST.Abstract mt.mmodifiers) && not(inlist AST.Abstract cl.cmodifiers)) then raise (InvalidModifier ("Can't define an abstract method in a non-abstract class, method: "^mt.mname^" of class: "^cl.cname));

  let imcFA = Modifiers.illegalModifierCombiFA in
  let imcNS = Modifiers.illegalModifierCombiNS in
  let imcPA = Modifiers.illegalModifierCombiPA in
  let imcSA = Modifiers.illegalModifierCombiSA in

  if (sub_list imcFA modifiers) then raise (IllegalModifierCombination ("Illegal modifier combinations: "^(join_list (List.map AST.stringOf_modifier imcFA) " & ")));
  if (sub_list imcNS modifiers) then raise (IllegalModifierCombination ("Illegal modifier combinations: "^(join_list (List.map AST.stringOf_modifier imcNS) " & ")));
  if (sub_list imcPA modifiers) then raise (IllegalModifierCombination ("Illegal modifier combinations: "^(join_list (List.map AST.stringOf_modifier imcPA) " & ")));
  if (sub_list imcSA modifiers) then raise (IllegalModifierCombination ("Illegal modifier combinations: "^(join_list (List.map AST.stringOf_modifier imcSA) " & ")))


let rec check_dup_args (argNames: string list) = 
	match argNames with
	| [] -> ()
	| hd::tl -> (
    if (inlist hd tl) then raise (DuplicateArgument("Dup Argument: "^hd));
    check_dup_args tl;
    ()
  )

let check_method_dup_args (args: AST.argument list) = 
  let argNames = List.map (fun (a: AST.argument) -> a.pident;) args in 
  check_dup_args argNames

let check_method_body_missing (cl: AST.astclass) (mt: AST.astmethod) = 
  let mebms = Modifiers.methodEmptyBodyModifiers in
  
  (* not abstract and native without body *)
  if ((mt.msemi) && (not(intersect_list mebms mt.mmodifiers))) then raise (MissingMethodBody("Missing method body, method: "^mt.mname^" of class: "^cl.cname))

let check_method_body_illegal_def (cl: AST.astclass) (mt: AST.astmethod) = 
  let mebms = Modifiers.methodEmptyBodyModifiers in
  (* abstract and native with body *)
  if ((not mt.msemi) && (intersect_list mebms mt.mmodifiers)) then raise (InvalidMethodBody((join_list (List.map AST.stringOf_modifier mebms) " or "^" methods can't have body")))

let check_method_body (cl: AST.astclass) (mt: AST.astmethod) = 
  check_method_body_missing cl mt;
  check_method_body_illegal_def cl mt

let check_class_method (cl: AST.astclass) (mt: AST.astmethod) = 
  check_method_modifiers mt.mmodifiers cl mt;
  check_method_dup_args mt.margstype;
  check_method_body cl mt

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
    if (inlist hd tl) then raise(DuplicateClassDefinition("Duplicate class definition: "^hd));
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

(* abstract inheritance *)
let is_implemented (methods: AST.astmethod list) (mt:AST.astmethod) = 
  try
    check_method_in_list methods mt;
    true
  with
  | DuplicateMethod e -> false

let add_unimpl_methods (pMethods: AST.astmethod list) (cMethods: AST.astmethod list) =
  List.append pMethods (List.filter (fun (m: AST.astmethod) -> inlist AST.Abstract m.mmodifiers;) cMethods)
  
let rec unimplemented_methods (cl: AST.astclass) =
	if cl.cid="Object" then [] 
	else (
		let res = unimplemented_methods (search_class cl.cparent cl.cscope) in
		let unimplMethods = List.filter (is_implemented cl.cmethods) res in
		add_unimpl_methods unimplMethods cl.cmethods
  )
  
let rec check_class_abstract_inheritance (cl: AST.astclass) = 
  if not (inlist AST.Abstract cl.cmodifiers) then
    if List.length (unimplemented_methods cl) > 0 then raise (InvalidClassDefinition ("Class: "^cl.cname^" must be abstract or implement inherited abstract methods."));
	List.map check_class_abstract_inheritance (get_classes cl.ctypes);
	()

(* class attributes *)
let rec check_dup_attrs (attrNames: string list) = 
  match attrNames with
	| [] -> ()
	| hd::tl -> (
    if (inlist hd tl) then raise (DuplicateAttribute("Dup Argument: "^hd));
    check_dup_attrs tl;
    ()
  )

let check_class_dup_attrs (attrs: AST.astattribute list) = 
  let attrNames = List.map (fun (attr: AST.astattribute) -> attr.aname;) attrs in
  check_dup_attrs attrNames

let check_class_attr_modifiers (attr: AST.astattribute) = 
  check_modifiers attr.amodifiers;
  let cmms = Modifiers.classMemberModifiers in
  if not (List.for_all (fun m -> inlist m cmms;) attr.amodifiers) then raise (InvalidModifier ("Invalid modifier for a class member."))
  
let check_class_attr_coherence (cl: AST.astclass) (attr: AST.astattribute) = 
  print_endline("attr coherence")
	
let rec check_class_attributes (cl: AST.astclass) = 
  print_endline("attr def");
  check_class_dup_attrs cl.cattributes;
  List.map check_class_attr_modifiers cl.cattributes;
  List.map (check_class_attr_coherence cl) cl.cattributes;
  List.map check_class_attributes (get_classes cl.ctypes);
  ()
(**)

(* overring methods *)
let is_overriding (cScope: AST.astclass list) (cMethods: AST.astmethod list) (pMethod: AST.astmethod) = 
  let res = List.filter (
		fun (cMethod: AST.astmethod) -> 
			if cMethod.mname=pMethod.mname then (
				if (List.length pMethod.margstype)=(List.length cMethod.margstype) then (
					let sameTypeCheckList = List.map2 (fun (a1: AST.argument) (a2: AST.argument) -> is_types_equal a1.ptype a2.ptype;) pMethod.margstype cMethod.margstype in
					if (List.for_all (fun x -> x) sameTypeCheckList) then
					(
						let pPublic = inlist AST.Public pMethod.mmodifiers in
            let pProtected = inlist AST.Protected pMethod.mmodifiers in
            let pStatic = inlist AST.Static pMethod.mmodifiers in
                        
						let cProtected = inlist AST.Protected cMethod.mmodifiers in
						let cPrivate = inlist AST.Private cMethod.mmodifiers in
						let cStatic = inlist AST.Static cMethod.mmodifiers in
            
            (* Check reduce visibility *)
						if ((pPublic && cProtected)||(pPublic && cProtected)||(pProtected && cPrivate)) 
              then raise(IllegalOverridingMethod("Method: "^cMethod.mname^", cant override method with reduced visibility."));

            (* Check same mod Static *)
            if ((pStatic && (not cStatic))||((not pStatic) && cStatic)) 
              then raise (IllegalOverridingMethod ("Overriding method: "^cMethod.mname^" must have the same staticity."));

            (* Check same return type *)
            if (not(is_child_class cScope cMethod.mreturntype pMethod.mreturntype)) 
              then raise (IllegalOverridingMethod ("Overriding method: "^pMethod.mname^" must have same return type."))
            else true

					) else false
				) else false
			) else false
	) cMethods in
	(List.length res) > 0

let rec check_overriding_methods (cl: AST.astclass): AST.astmethod list = 
  if cl.cid="Object" then cl.cmethods
  else (
    let res = check_overriding_methods (search_class cl.cparent cl.cscope) in
    let notOverridingMethods = List.filter (fun x -> (not(is_overriding cl.cscope cl.cmethods x));) res in
    notOverridingMethods@cl.cmethods
  )

let rec check_class_overriding_methods (cl: AST.astclass) = 
  check_overriding_methods cl;
	List.map check_class_overriding_methods (get_classes cl.ctypes);
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
  List.map check_class_abstract_inheritance classes;
  List.map check_class_methods classes;
  List.map check_class_overriding_methods classes;
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
        AST.cscope = Object.objectClass::classes;
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
        AST.cscope=Object.objectClass::next;
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


