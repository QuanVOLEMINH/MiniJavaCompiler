
type classheader = 
  {
    classmodifier : string;
    classkey      : string;
    identifier    : string;
  }

type compilationunit = 
  | PackageUnit of string * string 
  | ImportUnit of string * string
(*   | ClassUnit of classheader * expression *)

let string_of_compilationunit compileunit=
  match compileunit with
    | PackageUnit(keyword, pkg)  -> keyword ^ " " ^ pkg ^ "\n"
    | ImportUnit(keyword, pkg)  -> keyword ^ " " ^ pkg ^ "\n"
