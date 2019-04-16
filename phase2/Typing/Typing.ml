open AST

exception Class_exist of string

type sig_info ={
  return_type : Type.t;
  arg_list : argument list
}
type class_info = {
  cparent : Type.ref_type;
  cmethods : (string, sig_info) Hashtbl.t;
}

(* let add_method  *)

let add_class env c = 
  List.iter (add_method env) c.cmethods;

let add_type env type_elem =
  match type_elem.info with
    | Class c -> 
      print_endline type_elem.id;
      if (Hashtbl.mem env type_elem.id)
      then 
        (
          (*class already exist*)
          raise(Class_exist(type_elem.id));
        )
      else 
        (
          (*class not exist*)
          Hashtbl.add env type_elem.id {
            cparent     = c.cparent;
            cmethods    = Hashtbl.create 42; 
          };
          add_class env c;
        )
    | Inter -> print_endline "Interface founded"

let typage ast =
  let env = Hashtbl.create 42 in
    List.iter (add_type env) ast.type_list;