open AST

let rec get_classes (type_list : AST.asttype list): AST.astclass list = 
  []

let rec add_package (package_name: AST.qualified_name) (list_classes: AST.astclass list) = 
  []

let get_package (package_name : AST.qualified_name option) (list_classes: AST.astclass list) : AST.astclass list =
  match package_name with
  | None -> list_classes
  | Some pn -> (add_package pn list_classes)


let get_program_info (package_name : AST.qualified_name option) (list_classes : AST.astclass list) : AST.astclass list =
  let classes = get_package package_name list_classes in
  classes


let type_program (program : AST.t) = 
  let classes = get_program_info (program.package) (get_classes program.type_list) in
  program