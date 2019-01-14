%{
    open Expr
%}

(* Named by Java 1.6 Grammar Spec - Chapter 18 *)

(* Modifier *)
%token PUBLIC

(* Special *)
%token EOF
%token AT

(* Separators *)
%token SEMICOLON LPAR RPAR LBRAC RBRAC

%token <string> IDENT
%token <int> INT

%token CLASS

%start compilationUnit

%type <string> compilationUnit

(* PrefixOp *)
%token INCR

%token EQUAL

%token INTEGER

(* InfixOp *)
%token PLUS MINUS TIMES DIV MOD

%%

(* Identifier *)
identifier:
  | id=IDENT { id }

modifier:
  (*| a=annotation {a}*)
  | PUBLIC { "public" }

compilationUnit:
  | (*id=importDeclaration*) td=typeDeclaration EOF { td }

typeDeclaration:
  | cd=classDeclaration {cd}
  | SEMICOLON { ";" }

classDeclaration:
  | ncd=normalClassDeclaration { ncd }
  (*|EnumDeclaration*)

normalClassDeclaration:
  | m=modifier CLASS id=identifier (*[TypeParameters] [extends Type] [implements TypeList]*) cb=classBody { m^"class "^id^" "^cb }

classBody:
  | LPAR cbd=classBodyDeclaration RPAR { "{ "^cbd^" }" }

classBodyDeclaration:
  | SEMICOLON { ";" }
  | cmd=classMemberDeclaration {cmd}

classMemberDeclaration:
  | fd=fieldDeclaration { fd }
    
(* Field Declarations *)
fieldDeclaration:
  | mt=myType vdl=variableDeclarators SEMICOLON { mt^" "^vdl^";"}
  
block:
	  LPAR bss=blockStatements RPAR { " {\n"^bss^"\n}" }
	| LPAR RPAR { " {} "}

blockStatements:
  | bs=blockstatement { bs }

blockstatement:
  | lvds=localVariableDeclarationStatement { lvds }

localVariableDeclarationStatement:
  | (*[final]*) mt=myType vds=variableDeclarators { mt^" "^vds }

variableDeclarators:
  | vd=variableDeclarator { vd }

variableDeclarator:
  | vdi=variableDeclaratorId { vdi }
  | vdi=variableDeclaratorId EQUAL vi=variableInitializer { vdi^" = "^vi }

variableDeclaratorId:
  | id=identifier { id }

variableInitializer:
  | e=expression {e}

myType:
  | b=basicType { b }

basicType:
  | INTEGER {"int"}
  
expression:
  | ae=assignmentExpression { ae }

assignmentExpression:
  | a=assignment { a }

assignment:
  | lhs=leftHandSide ao=assignmentOperator ae=expression { lhs^" "^ao^" "^ae }

leftHandSide:
  | en=expressionName { en }

expressionName:
  | id=identifier { id }

assignmentOperator:
  | EQUAL { "=" }
