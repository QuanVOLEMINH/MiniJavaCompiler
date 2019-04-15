open AST

let rec get_classes (type_list : AST.asttype list): AST.astclass list = 
    match type_list with
    | [] -> []
    | hd::tl -> (
        let c = get_classes tl in 
        match hd.info with
        | Class cl ->  cl.cname <- hd.id; cl.cmodifiers <- hd.modifiers; cl::c
        | Inter ->  c
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

let get_package (package_name : AST.qualified_name option) (list_classes: AST.astclass list) : AST.astclass list =
  match package_name with
  | None -> list_classes
  | Some pn -> (add_package pn list_classes "")@list_classes


let get_program_info (package_name : AST.qualified_name option) (list_classes : AST.astclass list) : AST.astclass list =
  let classes = get_package package_name list_classes in
  List.iter (fun c -> AST.print_class "*--*" c) classes;
  classes


let type_program (program : AST.t) = 
  let classes = get_program_info (program.package) (get_classes program.type_list) in
  program