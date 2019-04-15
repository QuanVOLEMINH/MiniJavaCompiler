open AST

type method_info = {
  return_type : Type.t;
  args : argument list
}

type class_info = {
  parent : Type.ref_type;
  methods : (string, method_info) Hashtbl.t;
  constructors : (string, method_info) Hashtbl.t;
  attributes : (string, Type.t) Hashtbl.t;
}

type global = {
  classes : (string, class_info) Hashtbl.t ;
  mutable current : string
}

type scope = {
  return_type: Type.t;
  vars: (string, Type.t) Hashtbl.t
}

let type_val v =
  match v with
  | Int i -> Some(Type.Primitive(Type.Int))
  | Float f -> Some(Type.Primitive(Type.Float))
  | Char c -> Some(Type.Primitive(Type.Char))
  | String s -> Some(Type.Ref({ tpath = []; tid = "String" }))
  | Boolean b -> Some(Type.Primitive(Type.Boolean))
  | Null -> None


let type_program tp = let global = { classes = Hashtbl.create 10; current = "" } in 
  print_endline "ok"

let print_type_program tp =
  print_endline "OK"