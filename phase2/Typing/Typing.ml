open AST


let add_type env type_elem =
  match type_elem.info with
    | Class c -> 
      if (Hashtbl.mem env type_elem.info)
      then 
        (print_endline "already exist";
        )
      else 
        (print_endline "not exist";)
    | Inter -> print_endline "Interface founded"

let typage ast =
  let env = Hashtbl.create 42 in
    List.iter (add_type env) ast.type_list;