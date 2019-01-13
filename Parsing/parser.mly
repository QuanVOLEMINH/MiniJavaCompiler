%{
    open Expr
%}

(* Named by Java 1.6 Grammar Spec - Chapter 18 *)

(* Modifier *)
%token PUBLIC

(* InfixOp *)
%token PLUS MINUS TIMES DIV MOD

(* PrefixOp *)
%token INCR

(* Special *)
%token EOF
%token AT

(* Separators *)
%token SEMICOLON LPAR RPAR LBRAC RBRAC

%token EQUAL

%token INTEGER

%token <int> INT

%token <string> IDENT

%token CLASS

%start <string> compilationUnit

%%

(* Identifier *)
identifier:
  | id=IDENT { id }

compilationUnit:
  | (*id=importDeclaration*) td=typeDeclaration EOF { td }

typeDeclaration:
  | coid=classOrInterfaceDeclaration {coid}
  | SEMICOLON { ";" }

modifier:
  (*| a=annotation {a}*)
  | PUBLIC { "public" }

classOrInterfaceDeclaration:
  | m=modifier cd=classDeclaration { cd }

classDeclaration:
  | ncd=normalClassDeclaration { ncd }
  (*|EnumDeclaration*)

normalClassDeclaration:
  | CLASS id=identifier (*[TypeParameters] [extends Type] [implements TypeList]*) cb=classBody { "class "^id^" "^cb }

classBody:
  | LPAR cbd=classBodyDeclaration RPAR { "{ "^cbd^" }" }

classBodyDeclaration:
  | SEMICOLON { ";" }
  | b=block { b }
  | m=modifier md=memberDecl { m^" "^md}

memberDecl:
  | mofd=methodOrFieldDecl { mofd }

methodOrFieldDecl:
  | t=myType id=identifier mofr=methodOrFieldRest { t^" "^id^" "^mofr}

methodOrFieldRest:
  | vdr=variableDeclaratorRest { vdr }
  (*| MethodDeclaratorRest*)

variableDeclaratorRest:
  | EQUAL vi=variableInitializer { " = "^vi }

variableInitializer:
  (*| ArrayInitializer*)
  (*| e=expression { e }*)
  | id=identifier { id }

expression:
  | ae=assignmentExpression { ae }
  (*| e=expression1 { e }*)
  (*| e1=expression1 ao=assignmentOperator e2=expression1 { e1^" "^ao^" "^e2 }*)

assignmentExpression:
  | a=assignment { a }

assignment:
  |  lhs=leftHandSide ao=assignmentOperator ae=assignmentExpression { lhs^" "^ao^" "^ae }

leftHandSide:
  | en=expressionName { en }

expressionName:
  | id=identifier { id }

expression1:
  | e=expression2 { e }

expression2:
  | e=expression3 { e }

expression3:
  | po=prefixOperator e=expression3 { po^" "^e }
  | LBRAC mt=myType RBRAC e=expression3 { "("^mt^")"^e }

assignmentOperator:
  | EQUAL { "=" }

prefixOperator:
  | INCR { "++" }

myType:
  | b=basicType { b }

basicType:
  | INTEGER {"int"}

block:
  | bss=blockStatements { bss }

blockStatements:
  | bs=blockstatement { bs }

blockstatement:
  | lvds=localVariableDeclarationStatement { lvds }

localVariableDeclarationStatement:
  | (*[final]*) mt=myType vds=variableDeclarators SEMICOLON { mt^" "^vds^";" }

variableDeclarators:
  | vd=variableDeclarator { vd }

variableDeclarator:
  | id=identifier vdr=variableDeclaratorRest { id^" "^vdr }
