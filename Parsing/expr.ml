
type classheader = 
  {
    classmodifier : string;
    classkey      : string;
    identifier    : string;
  }

type compilationunit = 
  | PackageDeclaration of string 
  | ImportDeclaration of string


let string_of_compilationunit compileunit=
  match compileunit with
    | PackageDeclaration(pkg)  -> "package name: " ^ pkg ^ "\n"
    | ImportDeclaration(pkg)  -> "import type: " ^ pkg ^ "\n"
