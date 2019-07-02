open TypingHelper
open ExceptionsDef


let rec get_classes (innerClasses: AST.asttype list): AST.astclass list = 
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
  
let rec get_parents (cl: AST.astclass) : Type.ref_type list =
  if cl.cid="Object" then [Type.object_type]
  else (
    let par = search_class cl.cparent cl.cscope in
    ({Type.tpath=(TypingHelper.get_path cl.cid) ; Type.tid=cl.cname})::(get_parents par)
  )

let rec get_parent_classes (cl: AST.astclass): AST.astclass list=
  if cl.cid="Object" then [cl]
  else (
    let par = search_class cl.cparent cl.cscope in
    cl::(get_parent_classes par)
  )

let bigger_binary_size t1 t2 : Type.t =
  if t1=Type.Primitive Boolean || t2=Type.Primitive Boolean then raise (InvalidExpression ("Incompatible types."))
  else if t1=Type.Primitive Double || t2=Type.Primitive Double then Type.Primitive Double
  else if t1=Type.Primitive Float || t2=Type.Primitive Float then Type.Primitive Float
  else if t1=Type.Primitive Long || t2=Type.Primitive Long then Type.Primitive Long
  else if t1=Type.Primitive Int || t2=Type.Primitive Int then Type.Primitive Int
  else if t1=Type.Primitive Short || t2=Type.Primitive Short then Type.Primitive Short
  else if t1=Type.Primitive Char || t2=Type.Primitive Char then Type.Primitive Char
  else Type.Primitive Byte

(* get first dup type then return with rest list *)
let rec get_first_rtype_rep (rt: Type.ref_type) (l: Type.ref_type list) : bool*Type.ref_type*(Type.ref_type list) =
  match l with
  | [] -> (false, rt, l)
  | hd::tl -> 
    if TypingHelper.is_types_equal (Type.Ref rt) (Type.Ref hd) then (true, rt, tl)
    else get_first_rtype_rep rt tl

(* get first common type then return with rest list *)

let rec get_first_rtype_common (l1: Type.ref_type list) (l2: Type.ref_type list) : Type.ref_type*(Type.ref_type list) =
  match l1 with
  | [] -> raise (InvalidExpression("Not inherit"))
  | hd::tl ->
    let (found, elem, rest) = get_first_rtype_rep hd l2 in
    if (found) then (elem, rest)
    else get_first_rtype_common tl l2
  
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

let find_method (scope:AST.astclass list) (id:string) (args:Type.t list) (meth:AST.astmethod) : bool=
  if meth.mname = id then
    if (List.length args) = (List.length meth.margstype) then
      let res = List.map2 (fun x (y:AST.argument) -> is_child_class scope x y.ptype ) args meth.margstype in 
      List.for_all (fun x->x) res
    else false
  else
    false 

let rec find_method_in_list (callerCl: AST.astclass) (calledClMethods: AST.astmethod list) (id:string) (args: Type.t list) : AST.astmethod option = 
  match calledClMethods with
  | [] -> None
  | hd::tl -> 
    if find_method callerCl.cscope id args hd then
      Some hd
    else 
      find_method_in_list callerCl tl id args



let checkConsts (scope:AST.astclass list) (id:string) (args:Type.t list) (cons:AST.astconst) : bool =
  if cons.cname = id then (
    if (List.length args) = (List.length cons.cargstype) then
      let res = List.map2 (fun x (y:AST.argument) -> is_child_class scope x y.ptype ) args cons.cargstype in 
      List.for_all (fun x->x) res
    else false
  )
  else
    false

let rec find_constructor (caller: AST.astclass) (consts: AST.astconst list) (id: string) (args: Type.t list): AST.astconst option = 
	match consts with
	| [] -> None
	| hd::tl -> 
		if checkConsts caller.cscope id args hd then Some hd
		else find_constructor caller tl id args


    let rec check_type_method (callerCl: AST.astclass) (calledCl: AST.astclass) (id: string) (types: Type.t list): Type.t =
      if id ="super" then (
        let par = search_class callerCl.cparent calledCl.cscope in
        let par_t = Type.Ref {Type.tpath=(get_path par.cid);Type.tid=par.cname} in
        if ((List.length par.cconsts)=0) && ((List.length types)=0) then par_t
        else (
          let res = find_constructor callerCl par.cconsts par.cname types in 
          (
            match res with 
            | Some res -> (
              if (inlist AST.Private res.cmodifiers) then 
                raise (InvalidExpression("constructor is private"))
              else par_t
            )
            | None -> raise (InvalidExpression("constructor not found "))
          )
        )
      )
      else (
        let caller_t = Type.Ref {Type.tpath=(get_path callerCl.cid);Type.tid=callerCl.cname} in
        let called_t = Type.Ref {Type.tpath=(get_path calledCl.cid);Type.tid=calledCl.cname} in 	
        let res = find_method_in_list callerCl calledCl.cmethods id types in 
        
        match res with
        | Some res -> ( 
          if (inlist AST.Private res.mmodifiers) then (
            if TypingHelper.is_types_equal caller_t called_t then res.mreturntype
            else raise (InvalidExpression("method "^id^" is private"))
          )
          else (
            if (inlist AST.Protected res.mmodifiers) then (
              if is_child_class callerCl.cscope caller_t called_t then
              res.mreturntype
            else 
              raise (InvalidExpression("method "^id^" is protected"))
            )
            else res.mreturntype
          )
        )
        | None -> (
          let res = find_constructor callerCl calledCl.cconsts id types in
          match res with 
          | Some res -> (
            if (inlist AST.Private res.cmodifiers) then 
              if TypingHelper.is_types_equal caller_t called_t then
                called_t
              else 
                raise (InvalidExpression("constructor "^id^" is private"))
            else 
              if (inlist AST.Protected res.cmodifiers) then 
                if is_child_class callerCl.cscope caller_t called_t then               called_t
                else raise (InvalidExpression("constructor "^id^" is protected"))
              else called_t
          )
        )
        | None -> (
          if calledCl.cid = "Object" then raise (InvalidExpression("method "^id^" not found"))
          else 
            check_type_method callerCl (search_class calledCl.cparent calledCl.cscope) id types
        )
      )

let rec check_type_variable_stmt (id: string) (stmts: AST.statement list): Type.t option =
  match stmts with
  | [] -> None
  | hd::tl -> (
    match hd with
    | VarDecl t_s_eOption -> check_type_variable_decl id tl t_s_eOption
    | _ -> check_type_variable_stmt id tl
  )

and check_type_variable_decl (id: string) (stmts: AST.statement list) t_s_eOption: Type.t option =
  match t_s_eOption with
  | [] -> check_type_variable_stmt id stmts
  | (t, s, eOption)::tl -> if s=id then Some t else check_type_variable_decl id stmts tl

let rec check_type_variable_args (id:string) (args: AST.argument list): Type.t option =
  match args with
  | [] -> None
  | hd::tl -> if hd.pident=id then Some hd.ptype else check_type_variable_args id tl

let check_type_variable_attr (attr: AST.astattribute) (id: string) (base: bool): Type.t option =
  if base then (if attr.aname=id then Some attr.atype else None)
  else (
    if (not (inlist AST.Private attr.amodifiers)) || (inlist AST.Protected attr.amodifiers) then (
      if attr.aname=id then Some attr.atype else None
    )
    else None
  )
  
let rec check_type_variable_attrs (attrs: AST.astattribute list) (id: string) (base: bool): Type.t option =
  match attrs with
  | [] -> None
  | hd::tl -> (
    let res = check_type_variable_attr hd id base in 
    match res with
    | None -> check_type_variable_attrs tl id base
    | Some x -> Some x
  )

let rec type_variable_parents (classes: AST.astclass list) (id: string) (base: bool): Type.t option =
  match classes with
  | [] -> None
  | hd::tl -> (
    let res = check_type_variable_attrs hd.cattributes id base in 
    match res with 
    | None -> type_variable_parents tl id base
    | Some x -> Some x
  )

let check_type_variable_class (id: string) (cl: AST.astclass): Type.t option =
  let found = check_type_variable_attrs cl.cattributes id true in
  match found with
  | Some t -> Some t
  | None -> type_variable_parents (get_parent_classes cl) id false

let check_type_variable (id: string) (cl: AST.astclass) (args: AST.argument list) (stmts: AST.statement list): Type.t =
  if (id="this") then Type.Ref {Type.tpath=TypingHelper.get_path(cl.cid); Type.tid=cl.cname}
  else (
    if (id="super") then Type.Ref cl.cparent
    else (
      let stmtVars = check_type_variable_stmt id stmts in
      match stmtVars with
      | Some t -> t
      | None -> (
        match (check_type_variable_args id args) with
          | Some t -> t
          | None -> (
            match check_type_variable_class id cl with
              | Some t -> t
              | None ->( 
                try
                  let res = search_class {Type.tpath=[];Type.tid=id} cl.cscope in
                  Type.Ref {Type.tpath=TypingHelper.get_path(res.cid);Type.tid=id}
                with
                | _ -> raise (IdentifierNotFound ("Var: "^id^" not found."))
                )
          )
      )
    )
  )
    
let rec check_type_inference (classes: AST.astclass list) (types: Type.t list): Type.t*(Type.ref_type list) =
  (* Type.Void *)
  match types with 
  | [] -> raise (NotImplemented "Empty Array Init")
  | hd::[] -> ( 
    match hd with 
    | Ref f -> (hd, (get_parents (search_class f classes)))
    | _ -> (hd, [])
  )
  | hd::tl -> (
    let (t, parents) = check_type_inference classes tl in
    if (TypingHelper.is_types_equal hd t) then (t, parents)
    else (
      match t with
      | Array (tType, tDims) -> (
					match hd with
					| Array (hdType, hdDims) ->
						if (tDims <> hdDims) then raise (InvalidExpression("Array dimensions mismatch"))
						else (				
							match tType with
							| Ref rTType -> (
								match hdType with
								| Ref rHdType ->(
									let p1 = get_parents (search_class rTType classes) in
									let p2 = get_parents (search_class rHdType classes) in
									let (res1, res2) = get_first_rtype_common p1 p2 in
									(Type.Array(Type.Ref res1, hdDims), [])
								)
								| _ -> raise (InvalidExpression("Array type mismatch"))
							)
							| _ -> raise (InvalidExpression("Array type mismatch"))
						)
					| _ ->  raise (InvalidExpression("Array type mismatch"))
			)
      | Ref rt -> (
        match hd with
        | Ref rhd -> (
          let pars = get_parents (search_class rhd classes) in
          let (elem, rest) = get_first_rtype_common pars parents in
          (Type.Ref elem, rest)
        )
        | _ ->  raise (InvalidExpression("Array type mismatch"))
      )
      | _ ->  raise (InvalidExpression("Array type mismatch"))
    )
  )
      
let rec check_type_expression (cl: AST.astclass) (args: AST.argument list) (stmts: AST.statement list) (expr: AST.expression): Type.t = 
  (* print_endline "expr"; *)
  match expr.etype with
  | Some x -> x
  | None -> (
    let res = (
    match expr.edesc with 
      | AST.New (sOption, sList, eList) -> 
        (
          let (hd, tl) = TypingHelper.get_last sList in
          check_expr_new cl {Type.tpath = hd; Type.tid = tl} (List.map (check_type_expression cl args stmts) eList);
          Type.Ref {Type.tpath = hd; Type.tid = tl}
        )	
      | AST.NewArray (t, eOptionList, eOption) -> 
        (
          let (dims, dimsDecl) = check_array_dims cl args stmts eOptionList in
          (* print_endline ("-"^(string_of_int dims)^" - "^(string_of_int dimsDecl)); *)
          ( 
            match eOption with
            | None -> ()
            | Some e1 -> ( 
              match e1.edesc with
              | AST.ArrayInit (l) -> 
                if (dimsDecl > 0) then raise (InvalidExpression("Cannot define dimension expressions when an array initializer is provided."))
                else(
                  let res = check_type_expression cl args stmts e1 in 
                  match res with
                  | Type.Array (at, d) -> ( 
                    (* print_endline ("--"^(string_of_int d)); *)
                    if (d <> dims) then raise (InvalidExpression("Array dimensions mismatch"^(string_of_int d)^
                    " != "^(string_of_int dims)));
                    if not (is_child_class cl.cscope at t) then raise (InvalidExpression("Array type mismatch"));
                  )  
                  | _ -> raise (InvalidExpression("Invalid new array."))
                )   
              )
          );
          Type.Array (t, dims)
        )
      | AST.Call (eOption, s, eList) -> (
        print_endline "Call";
        match eOption with
        | None ->
          check_type_method cl cl s (List.map (check_type_expression cl args stmts) eList) 
        | Some x -> (
          let t = check_type_expression cl args stmts x in
          (
            match t with
            | Ref r ->
              (
                let cls = search_class r cl.cscope in 
                check_type_method cl cls s (List.map (check_type_expression cl args stmts) eList)
              )
            | _ -> raise (InvalidExpression("Type mismatched: "^(Type.stringOf t)))
          )
        )
      )
      | AST.Attr (e, s) -> (
        print_endline "Attr"; 
        let attr_res = check_type_expression cl args stmts e in
               
        match attr_res with
        | Ref r -> ( 
          let cls = search_class r cl.cscope in 
          check_type_variable s cls args stmts
        )
        | Array (typ, size) -> (
          if s="length" then Type.Primitive Type.Int 
          else raise (InvalidExpression("Type mismatched: "^(Type.stringOf attr_res)))
        )
        | _ -> raise (InvalidExpression("Type mismatched: "^(Type.stringOf attr_res)))
        
      )
      | AST.If (e1, e2, e3) -> (
        let t1 = check_type_expression cl args stmts e1 in 
        let t2 = check_type_expression cl args stmts e2 in 
        let t3 = check_type_expression cl args stmts e3 in 
        (* If expression must be boolean *)
				if(not(is_types_equal t1 (Type.Primitive Type.Boolean))) then 
					raise (InvalidExpression("Missing boolean expr in If."))
				else 
					let (t, p) = check_type_inference cl.cscope (t2::t3::[]) in t
      )
      | AST.Val v -> (
        match v with
          | String s -> Type.Ref {tpath = []; tid = "String"}
          | Int i -> Type.Primitive Int
          | Float f -> Type.Primitive Float
          | Char c -> Type.Primitive Char
          | Null -> Type.Ref { tpath = []; tid = "Null" }
          | Boolean b -> Type.Primitive Boolean
      )
      | AST.Name n -> (
        (* print_endline "name"; *)
        (check_type_variable n cl args stmts)
      )
      | AST.ArrayInit eList -> (
        let (t, parents) = check_type_inference cl.cscope (List.map (check_type_expression cl args stmts) eList) in 
      
        match t with 
        | Type.Array (at, dims) -> Type.Array (at, dims + 1) 
        | _ -> Type.Array (t, 1) 
      )
      | AST.Array (e, eOptionList) -> print_endline "Array"; Type.Void
      | AST.AssignExp (e1, assign_op, e2) -> (
        print_endline "AssignExp";
        let t1=check_type_expression cl args stmts e1 in
        let t2=check_type_expression cl args stmts e2 in
        
        if (is_types_equal t1 t2) then t1
        else (
          match assign_op with
          | Ass_add -> 
            if is_child_class cl.cscope t1 (Type.Ref {Type.tpath=[]; Type.tid="String"}) then
              match t2 with
              | Type.Primitive p -> t1
              | Type.Ref f -> (
                if is_child_class cl.cscope t2 (Type.Ref {tpath=[]; tid="String"}) then t1 
                else
                  raise (InvalidExpression("Assignment type mismatch "^(Type.stringOf t1)^" != "^(Type.stringOf t2)))
              )
              | _ -> raise (InvalidExpression("Assignment type mismatch "^(Type.stringOf t1)^" != "^(Type.stringOf t2)))
            else
              raise (InvalidExpression("Assignment type mismatch "^(Type.stringOf t1)^" != "^(Type.stringOf t2)))
          | _ -> raise (InvalidExpression("Assignment type mismatch "^(Type.stringOf t1)^" != "^(Type.stringOf t2)))
        )
      )
      | AST.Post (e, postfix_op)  -> print_endline "Post"; Type.Void
      | AST.Pre (prefix_op, e) -> print_endline "Pre"; Type.Void
      | AST.Op (e1, infix_op, e2) -> (
        print_endline "Op";
        let t1=check_type_expression cl args stmts e1 in
        let t2=check_type_expression cl args stmts e2 in
        let logicOperators = Operators.logicOperators in 
				if (t1=(Type.Primitive Boolean) && t2=(Type.Primitive Boolean)) then (
          (* logic op *)
          if inlist infix_op logicOperators then t1
          else raise (InvalidExpression ("Invalid operation "^(AST.string_of_infix_op infix_op)^" for type "^(Type.stringOf t1)^", "^(Type.stringOf t2)^"."))
				) else (
					let primTypes = TypingHelper.primitiveTypes in
					if ((inlist t1 primTypes) && (inlist t2 primTypes)) then (
            (* compare op *)
            let compaOps = Operators.compareOperators in
            if inlist infix_op compaOps then (Type.Primitive Type.Boolean)
            else (
              let calculOps = Operators.calculOperators in
							if inlist infix_op calculOps then bigger_binary_size t1 t2
							else raise (InvalidExpression ("Invalid operation "^(AST.string_of_infix_op infix_op)^" for type "^(Type.stringOf t1)^", "^(Type.stringOf t2)^"."))
						)
					) else (
            let isT1String = is_child_class cl.cscope t1 (Type.Ref {tpath=[]; tid="String"}) in
            let isT2String = is_child_class cl.cscope t2 (Type.Ref {tpath=[]; tid="String"}) in
						if (isT1String) || (isT1String) then (
              (* add 2 strings *)
							if infix_op=Op_add then Type.Ref {tpath=[]; tid="String"}
							else raise (InvalidExpression ("Invalid operation "^AST.string_of_infix_op infix_op^" for type "^Type.stringOf t1^", "^Type.stringOf t2^"."))
						) else (
							raise (InvalidExpression ("Invalid operation "^AST.string_of_infix_op infix_op^" for type "^Type.stringOf t1^", "^Type.stringOf t2^"."))
						)
					)
				) 
      )
      | AST.CondOp (e1, e2, e3) -> (
        print_endline "CondOp";
        let t1 = check_type_expression cl args stmts e1 in 
				let t2 = check_type_expression cl args stmts e2 in 
				let t3 = check_type_expression cl args stmts e3 in 
        if(not(is_types_equal t1 (Type.Primitive Type.Boolean))) then 
          raise (InvalidExpression("Invalid conditional expr"))
				else 
					let (t,p) = check_type_inference cl.cscope (t2::t3::[]) in t
      )
      | AST.Cast (t, e) -> print_endline "Cast"; t
      | AST.Type t -> print_endline "Type"; t
      | AST.ClassOf t -> (
        print_endline "ClassOf";  
        Type.Ref {Type.tpath=[]; Type.tid="Class"}
      )
      | AST.Instanceof (e, t) -> (
        let typ = check_type_expression cl args stmts e in (Type.Primitive Type.Boolean)
      )
      | AST.VoidClass -> print_endline "VoidClass"; Type.Void
      ) in 
      (
        expr.etype <- Some res; res
      )
  )

and check_expr_new (cl: AST.astclass) (t: Type.ref_type) (types: Type.t list) =
  let checkClass = search_class t cl.cscope in
  if (List.length types = 0) && (List.length checkClass.cconsts = 0) then () 
  else (
    let checkConsts = List.map (
      fun (c: AST.astconst) -> if (List.length c.cargstype)=(List.length types) then
        (
          let equalTypesList = List.map2 (fun (a: AST.argument) (tp: Type.t) -> is_child_class checkClass.cscope a.ptype tp;) c.cargstype types in
          
          if (List.for_all (fun x -> x) equalTypesList) then Some c else None		
        ) else None;
    ) checkClass.cconsts in
    
    if List.for_all (
      fun (c: AST.astconst option) -> match c with
            | None -> true
            | Some c -> (
              if (inlist AST.Private c.cmodifiers) && c.cname <> cl.cname then true else false;
              if (inlist AST.Protected c.cmodifiers) && not 
              (is_child_class cl.cscope (Type.Ref {Type.tpath=(TypingHelper.get_path cl.cid); Type.tid=cl.cname}) (Type.Ref{Type.tpath=(TypingHelper.get_path checkClass.cid); Type.tid=checkClass.cname})) then true else false		      
            )
      ) checkConsts then raise (InvalidConstructorDefinition ("Cant find constructor signature of class: "^checkClass.cname^" with these arguments."))
  )
  
and check_array_dims (cl: AST.astclass) (args: AST.argument list) (stmts: AST.statement list) (eOptionList: (AST.expression option) list): int*int =
  let res = List.map (
    fun (eOption: AST.expression option) -> 
      match eOption with 
        | None -> false
        | Some eOpt -> 
          let t = check_type_expression cl args stmts eOpt in
          match t with 
          | Primitive p -> (
            match p with
            | Int -> true
            | _ ->  raise (InvalidExpression("Array dimensions must be ints."))
          )
          | _ -> raise (InvalidExpression("Array dimensions must be ints."))
    ) eOptionList in
  
  ((List.length res), (get_decl_dims res 0 true))

and get_decl_dims (res: bool list) (size: int) (v: bool) : int=
  match res with
  | [] -> (size)
  | hd::tl -> 
    if v then
      if hd then get_decl_dims tl (size + 1) true
      else get_decl_dims tl size false
    else 
      if hd then raise (InvalidExpression("Cannot specify an array dimension after an empty dimension."))
      else get_decl_dims tl size false

let rec check_type_statements (cl: AST.astclass) (args: AST.argument list) (rt: Type.t)(checkedStmts: AST.statement list) (uncheckedStmts: AST.statement list) =
  match uncheckedStmts with 
  | [] -> ()
  | hd::tl -> (
    (
      match hd with
      | AST.VarDecl t_s_eOption_List -> check_var_declaration cl args checkedStmts t_s_eOption_List; ()
      | AST.Block stmt_List -> print_endline "block"; check_type_statements cl args rt checkedStmts stmt_List; ()
      | AST.Nop -> print_endline "Nop"
      | AST.While (e, stmt) -> (
        print_endline "While";
        if not (is_types_equal (check_type_expression cl args checkedStmts e) (Type.Primitive Type.Boolean)) then
          raise (InvalidExpression("Missing boolean expr in While."))
				else check_type_statements cl args rt checkedStmts [stmt]; ()
      )
      | AST.For (tOption_s_eOption_List, eOption, eList, stmt) -> raise (NotImplemented("For: not implemented"))
      | AST.If (e, stmt, stmtOption) -> (
        print_endline "If";
        if not (is_types_equal (check_type_expression cl args checkedStmts e) (Type.Primitive Type.Boolean)) then 
          raise (InvalidExpression("Missing boolean expr in If."))
				else 
					(
						check_type_statements cl args rt checkedStmts [stmt];
						match stmtOption with
						| None -> ()
						| Some st -> check_type_statements cl args rt checkedStmts [st]; ()
					)
      )
      | AST.Return eOption -> (
        print_endline "Return";
        match eOption with
				| None -> 
					if is_types_equal rt Type.Void then () 
					else raise (InvalidStatement("Return type must be empty."))
				| Some eOpt -> 
					if is_child_class cl.cscope (check_type_expression cl args checkedStmts eOpt) rt  then () 
					else raise (InvalidStatement("Return type must be of: "^(Type.stringOf rt)))
      )
      | AST.Throw e -> (
        print_endline "Throw";
        check_type_expression cl args checkedStmts e; 
        () 
      )
      | AST.Try (stmtList1, arg_stmtList_List, stmtList2) -> (
        print_endline "Try";
        check_type_statements cl args rt checkedStmts stmtList1;
				List.iter (fun (arg, st) -> check_type_statements cl args rt checkedStmts st ) arg_stmtList_List;
        check_type_statements cl args rt checkedStmts stmtList2;
        ()
      )
      | AST.Expr e -> (check_type_expression cl args checkedStmts e; ())  
    );
    check_type_statements cl args rt (checkedStmts@[hd]) tl
  )
 
and check_var_declaration (cl: AST.astclass) (args: AST.argument list) (stmts: AST.statement list) (varDecls: (Type.t * string * AST.expression option) list) =
  match varDecls with 
  | [] -> ()
  | (t, varId, eOption)::tl -> (
    match eOption with
    | None -> ()
    | Some e -> 
      let te = check_type_expression cl args stmts e in
      if (is_child_class cl.cscope te t) then check_var_declaration cl args stmts tl
      else raise (InvalidStatement("Variable: "^varId^", type mismatch "^(Type.stringOf t)^" != "^(Type.stringOf te)))
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

(* Class member modifier *)
let rec check_class_inner_member_modifiers (allowStatic: bool) (cl: AST.astclass) =
	if (not allowStatic) && (inlist AST.Static cl.cmodifiers) then raise (InvalidModifier("Static members can only be declared in static class or top level members."));
  
  List.map (check_class_inner_member_modifiers (inlist AST.Static cl.cmodifiers)) (get_classes cl.ctypes);
	()

let check_class_member_modifiers (cl: AST.astclass) = 
  if (inlist AST.Private cl.cmodifiers) then raise (InvalidModifier("Private modifier is not allowed for top level class declarations."));
  if (inlist AST.Protected cl.cmodifiers) then raise (InvalidModifier("Protected modifier is not allowed for top level class declarations."));
  if (inlist AST.Static cl.cmodifiers) then raise (InvalidModifier("Static modifiers is not allowed for top level class declarations. Every top level class is static"));

	List.map (fun (t: AST.asttype) -> 
				match t.info with
				 | Class cl -> check_class_inner_member_modifiers true cl
				 | _ -> ()
			) cl.ctypes;
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
  check_method_body_illegal_def cl mt;
  check_type_statements cl mt.margstype mt.mreturntype [] mt.mbody


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
  match attr.adefault with
	| None -> ()
	| Some at -> 
		if is_child_class cl.cscope (check_type_expression cl [] [] at ) attr.atype then ()
		else raise (InvalidStatement("Invalid attribute type initalization: "^(Type.stringOf attr.atype)^" != "^(Type.stringOf (check_type_expression cl [] [] at ))))

	
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
						if ((pPublic && cProtected)||(pPublic && cPrivate)||(pProtected && cPrivate)) 
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
    let notOverridingMethods = List.filter (fun m -> not(is_overriding cl.cscope cl.cmethods m);) res in
    let notPrivateMethods = (List.filter (fun (m: AST.astmethod) -> not(inlist AST.Private m.mmodifiers)) cl.cmethods) in
    notOverridingMethods@notPrivateMethods
  )

let rec check_class_overriding_methods (cl: AST.astclass) = 
  check_overriding_methods cl;
	List.map check_class_overriding_methods (get_classes cl.ctypes);
	()
 
(**)

(* class constructors *)
let check_class_constructor_body (cl: AST.astclass) (const: AST.astconst) =
  (* print_endline "constructor body"; *)
  check_type_statements cl const.cargstype Type.Void [] const.cbody

let check_class_constructor_modifiers (modifiers: AST.modifier list) = 
  check_modifiers modifiers;
  let accessModifiers = Modifiers.accessModifiers in
  if not(List.for_all (fun m -> (inlist m accessModifiers);) modifiers) then raise (InvalidModifier ("Constructor invalid modifier."))

let check_dup_constructors (c: AST.astconst) (consts: AST.astconst list) =
  List.iter (
    fun (ctr: AST.astconst) -> if ctr.cname=c.cname then
    (
      (* same argument *)
      if (List.length c.cargstype)=(List.length ctr.cargstype) then
      (
        (* same argument type *)
        let equalTypesList = List.map2 (fun (a1: AST.argument) (a2: AST.argument) -> TypingHelper.is_types_equal a1.ptype a2.ptype;) c.cargstype ctr.cargstype in
        if (List.for_all (fun x -> x) equalTypesList) then raise (DuplicateConstructor ("Constructor:  "^ctr.cname))
      )
    );
  ) consts  

let rec check_class_dup_constructors (consts: AST.astconst list) = 
  match consts with
	| [] -> ()
	| hd::tl -> check_dup_constructors hd tl; check_class_dup_constructors tl; ()

let check_class_constructor_name (cl: AST.astclass) =
  List.map (
    fun (c: AST.astconst) -> if c.cname <> cl.cname then raise (InvalidConstructorDefinition("Constructor name: "^c.cname^" and class name: "^cl.cname^" must be the same."));
  ) cl.cconsts
 
  
let rec check_class_constructors (cl: AST.astclass) = 
  check_class_constructor_name cl;
  check_class_dup_constructors cl.cconsts;
  List.map (fun (c: AST.astconst) -> check_class_constructor_modifiers c.cmodifiers) cl.cconsts;
  (* Check dup args like methods *)
	List.map (fun (c: AST.astconst) -> check_method_dup_args c.cargstype) cl.cconsts;
	List.map (check_class_constructor_body cl) cl.cconsts;
	List.map check_class_constructors (get_classes cl.ctypes);
  ()
(**)

(* class initialization *)
let rec check_class_initializations (cl: AST.astclass) = 
  (* print_endline("initialization def - "^cl.cname); *)
  List.map (fun (ini: AST.initial)-> check_type_statements cl [] Type.Void [] ini.block) cl.cinits;
	List.map check_class_initializations (get_classes cl.ctypes);
  ()
(**)

let rec check_classes (prog: AST.t) (classes: AST.astclass list) =
  check_dup_class prog.type_list;
  List.map check_class_modifiers classes;
  List.map check_class_member_modifiers (get_classes prog.type_list);
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
  Object.objectClass::BaseClass.stringClass::classesList




(* starting point *)
let type_program (program: AST.t) = 
  let classes = get_program_info (program.package) (get_classes program.type_list) in
  (* List.iter (fun c -> print_class_name c) classes; *)
  get_scopes (get_package_info program) classes classes;
  check_classes program classes;
  program


