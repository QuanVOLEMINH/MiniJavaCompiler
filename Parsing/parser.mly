%{
    open Expr
%}

(* Named by Java 1.6 Grammar Spec - Chapter 18 *)

(* Modifier *)
%token PUBLIC FINAL

(* Type *)
%token VOID

(* keywords *)
%token THIS SUPER

(* Special *)
%token EOF
%token AT

%token BOOLEAN

(* Separators *)
%token SEMICOLON COMMA COLON POINT LPAR RPAR LBRAC RBRAC LSBRAC RSBRAC

%token <string> IDENT
%token <int> INTEGER

%token CLASS

(* Op *)
%token INCR DECR 

%token EQUAL

(* integralType *)
%token BYTE SHORT INT LONG CHAR

(* floatingPointType *)
%token FLOAT DOUBLE

(* InfixOp *)
%token PLUS MINUS TIMES DIV MOD LT GT

(* digits *)
%token ZERODIGIT
%token <char> NONZERODIGIT

%start compilationUnit
%type <string> compilationUnit

%%

(* Identifier *)
identifier:
  | id=IDENT { id }

(* Modifiers *)
modifier:
  (*| a=annotation {a}*)
  | PUBLIC { "public" }

fieldModifiers:
  | fm=fieldModifier { fm }
  | fms=fieldModifiers fm=fieldModifier { fms^" "^fm }

fieldModifier:
  | PUBLIC { "public" }
  (*Annotation public protected private
  static
  final
  transient
  volatile*)

constructorModifiers:
  | cm=constructorModifier { cm }
  | cms=constructorModifiers cm=constructorModifier { cms^" "^cm }

constructorModifier:
  | PUBLIC {"public"}
(*Annotation protected private*)

variableModifiers:
  | vm=variableModifier { vm }
  | vms=variableModifiers vm=variableModifier { vms^" "^vm }

variableModifier:
  | FINAL { "final" }
  (*|Annotation*)

typeParameters:
  | LT tp1=typeParameter LPAR COMMA tp2=typeParameter RPAR GT { "<"^tp1^"{,"^tp2^"}>" }

typeParameter:
  | id=identifier { id }

simpleTypeName:
  | id=identifier { id } (* must be class name => to do *)

formalParameterList:
 | lfp=lastFormalParameter { lfp }
 | fps=formalParameters COMMA lfp=lastFormalParameter { fps^" , "^lfp }

formalParameters:
  | fp=formalParameter { fp }
  | fps=formalParameters COMMA fp=formalParameter { fps^" , "^fp}

formalParameter:
  | mt=myType vdi=variableDeclaratorId { mt^" "^vdi }
  | vms=variableModifiers mt=myType vdi=variableDeclaratorId { vms^" "^mt^" "^vdi }

lastFormalParameter:
  | vms=variableModifiers vdi=variableDeclaratorId { vms^" "^vdi}
  | vms=variableModifiers mt=myType vdi=variableDeclaratorId { vms^" "^mt^" "^vdi}
  | fp=formalParameter { fp }

(* Comment *)
(*comment:
  | tc=traditionalComment { tc }
  | ec=endOfLineComment { ec }

traditionalComment:
  | DIV TIMES ct=commentTail { "/*"^ct }

endOfLineComment:
  | DIV DIV { "//" }
  | DIV DIV cil=charactersInLine { "//"^cil}

commentTail:
  | TIMES cts=commentTailStar { "*"^cts }
  | ns=notStar ct=commentTail { ns^" "^ct }

commentTailStar:
  | DIV { "/" }
  | TIMES cts=commentTailStar { "*"^cts }
  | nsns=notStarNotSlash ct=commentTail { nsns^" "^ct }

notStar:
  | ic=inputCharacter but not *
  | lt=lineTerminator { lt }

notStarNotSlash:
  | ic=inputCharacter but not * or /
  | lt=lineTerminator { lt }

charactersInLine:
  | ic=inputCharacter { ic }
  | cil=charactersInLine ic=inputCharacter { cil^" "^ic }
*)



(* Starting point *)
compilationUnit:
  | (*id=importDeclaration*) td=typeDeclarations EOF { td }



typeDeclarations:
  | td=typeDeclaration { td }
  | tds=typeDeclarations td=typeDeclaration { tds^" "^td}

typeDeclaration:
  | cd=classDeclaration {cd}
  (*| id=intefaceDeclaration {id} *)
  | SEMICOLON { ";" }

classDeclaration:
  | ncd=normalClassDeclaration { ncd }
  (*|ed=EnumDeclaration { ed }*)

normalClassDeclaration:
  | CLASS id=identifier cb=classBody { "class "^id^" "^cb }
  | m=modifier CLASS id=identifier (*[TypeParameters] [extends Type] [implements TypeList]*) cb=classBody { m^" class "^id^" "^cb }

classBody:
  | LPAR cbds=classBodyDeclarations RPAR { "{\n "^cbds^" \n}\n" }

classBodyDeclarations:
  | cbd=classBodyDeclaration { cbd }
  | cbds=classBodyDeclarations cbd=classBodyDeclaration {cbds^" "^cbd}

classBodyDeclaration:
  | cmd=classMemberDeclaration { cmd }
  (* InstanceInitializer  
StaticInitializer
  *)
  | cd=constructorDeclaration { cd }

classMemberDeclaration:
  | fd=fieldDeclaration { fd }
  | md=methodDeclaration { md }
  | cd=classDeclaration { cd }
  (*InterfaceDeclaration
  *)
  | SEMICOLON { ";" }
    
fieldDeclaration:
  | mt=myType vdl=variableDeclarators SEMICOLON { mt^" "^vdl^";\n"}
  | fds=fieldModifiers mt=myType vdl=variableDeclarators SEMICOLON { fds^" "^mt^" "^vdl^";\n"}

variableDeclarators:
  | vd=variableDeclarator { vd }
  | vds=variableDeclarators COMMA vd=variableDeclarator { vds^" , "^vd }

variableDeclarator:
  | vdi=variableDeclaratorId { vdi }
  | vdi=variableDeclaratorId EQUAL vi=variableInitializer { vdi^" = "^vi }

variableDeclaratorId:
  | id=identifier { id }
  | vdi=variableDeclaratorId LSBRAC RSBRAC { vdi^"[]" }

variableInitializer:
  | e=expression {e}
  (*|ArrayInitializer*)

constructorDeclaration:
  | cd=constructorDeclarator cb=constructorBody { cd^" "^cb }
  | cms=constructorModifiers cd=constructorDeclarator cb=constructorBody { cms^" "^cd^" "^cb }

constructorDeclarator:
  | stn=simpleTypeName LBRAC RBRAC { stn^"()" }
  | tps=typeParameters stn=simpleTypeName LBRAC RBRAC { tps^" "^stn^"()" }
  | stn=simpleTypeName LBRAC fpl=formalParameterList RBRAC { stn^"("^fpl^")" }
  | tps=typeParameters stn=simpleTypeName LBRAC fpl=formalParameterList RBRAC { tps^" "^stn^"("^fpl^")" }

constructorBody:
  | LPAR RPAR { "{}\n" }
  (*| LPAR ExplicitConstructorInvocation opt BlockStatements opt RPAR*)

methodDeclaration:
  | mh=methodHeader mb=methodBody { mh^" "^mb }

methodHeader:
  | rt=resultType md=methodDeclarator { rt^" "^md }
  (*| TypeParameters ResultType MethodDeclarator*)

methodDeclarator:
  | id=identifier LBRAC RBRAC { id^"()" }
  | id=identifier LBRAC fpl=formalParameterList RBRAC { id^"("^fpl^")" }

methodBody:
  | b=block { b }
  | SEMICOLON { ";\n" }


(* Blocks *)
block:
	| LPAR RPAR { "{}\n"}
  | LPAR bss=blockStatements RPAR { "{\n  "^bss^"\n }\n" }

blockStatements:
  | bs=blockStatement { bs }
  | bss=blockStatements bs=blockStatement { bss^" "^bs }


blockStatement:
  | lvds=localVariableDeclarationStatement { lvds }
  | cd=classDeclaration { cd }
  | st=statement { st }

localVariableDeclarationStatement:
  | lvd=localVariableDeclaration SEMICOLON { lvd^";" }

localVariableDeclaration:
  | mt=myType vds=variableDeclarators { mt^" "^vds }
  | vms=variableModifiers mt=myType vds=variableDeclarators { vms^" "^mt^" "^vds }

statement:
  | swtbs=statementWithoutTrailingSubstatement { swtbs }
  | ls=labeledStatement { ls }
(*IfThenStatement
IfThenElseStatement
WhileStatement
ForStatement*)

statementWithoutTrailingSubstatement:
  | b=block { b }
  | es=emptyStatement { es }
  | es=expressionStatement { es }

labeledStatement:
| id=identifier COLON s=statement { id^":"^s }

emptyStatement:
  | SEMICOLON { ";" }
expressionStatement:
  | se=statementExpression SEMICOLON { se^";" }

statementExpression:
  | a=assignment { a }
  | pie=preIncrementExpression { pie }
  | pde=preDecrementExpression { pde }
  | pie=postIncrementExpression { pie }
  | pde=postDecrementExpression { pde }
(*MethodInvocation
ClassInstanceCreationExpression
  *)

preIncrementExpression:
  | INCR ue=unaryExpression { "++"^ue }

preDecrementExpression:
  | DECR ue=unaryExpression { "--"^ue }

postIncrementExpression:
  | pfe=postfixExpression INCR { pfe^"++" }

postDecrementExpression:
  | pfe=postfixExpression DECR { pfe^"--" }

unaryExpression:
  | pie=preIncrementExpression { pie }
  | pde=preDecrementExpression { pde }
  | PLUS ue=unaryExpression { "+"^ue }
  | MINUS ue=unaryExpression { "-"^ue }
  | uenpm=unaryExpressionNotPlusMinus { uenpm }

unaryExpressionNotPlusMinus:
  | pe=postfixExpression { pe }
  | ce=castExpression { ce }

postfixExpression:
  | en = expressionName { en }

castExpression: 
  | LBRAC pt=primitiveType RBRAC ue=unaryExpression { "("^pt^")"^ue }
  (*( ReferenceType ) UnaryExpressionNotPlusMinus*)

expression:
  | ae=assignmentExpression { ae }

assignmentExpression:
  | a=assignment { a }

assignment:
  | lhs=leftHandSide ao=assignmentOperator ae=expression { lhs^" "^ao^" "^ae }

leftHandSide:
  | en=expressionName { en }
  | fa=fieldAccess { fa }
  (* arrayAccess*)

assignmentOperator:
  | EQUAL { "=" }
    (*=
  *=
  /=
  %=
  +=
  -=
  <<=
  >>=
  >>>=
  &=
  ^=
  |=*)

expressionName:
  | id=identifier { id }
  | an=ambiguousName POINT id=identifier { an^"."^id }

ambiguousName:
  | id=identifier { id }
  | an=ambiguousName POINT id=identifier { an^"."^id }

primary:
  | pnna=primaryNoNewArray { pnna }
  (*ArrayCreationExpression*)

primaryNoNewArray:
  | l=literal { l }
  | mt=myType POINT CLASS { mt^".class" }
  | VOID POINT CLASS { "void.class" }
  | THIS { "this" }
  (*ClassName .this*)
  | LBRAC e=expression RBRAC { "("^e^")" }
  (*ClassInstanceCreationExpression*)
  | fa=fieldAccess { fa }
  (*MethodInvocation
    ArrayAccess*)

literal:
  | i=integerLiteral { i }
(*FloatingPointLiteral
BooleanLiteral
CharacterLiteral
StringLiteral
NullLiteral*)

integerLiteral:
  | dil=decimalIntegerLiteral { dil }
(*HexIntegerLiteral
OctalIntegerLiteral*)

decimalIntegerLiteral:
  | dn=decimalNumeral { dn }
  (*| dn=decimalNumeral its=integerTypeSuffix { dn^" "^its }*)

decimalNumeral:
  | ZERODIGIT { "0" }
  | nzd=NONZERODIGIT { String.make 1 nzd }
  | nzd=NONZERODIGIT ds=digits { (String.make 1 nzd)^ds }

digits:
  | d=digit { d }
  | ds=digits d=digit { ds^d }

digit:
  | ZERODIGIT { "0" }
  | nzd=NONZERODIGIT { (String.make 1 nzd) } 


(*integerTypeSuffix:*)

fieldAccess:
  | p=primary POINT id=identifier { p^"."^id }
  | SUPER POINT id=identifier { "super."^id }
  (*ClassName .super . Identifier*)

arrayAccess:
  | en=expressionName LSBRAC e=expression RSBRAC { en^"["^e^"]" }
  | pnna=primaryNoNewArray LSBRAC e=expression RSBRAC { pnna^"["^e^"]" }
 
(* Type *)
primitiveType:
  | nt=numericType { nt }
  | BOOLEAN { "boolean" }

referenceType:
  | coit=classOrInterfaceType { coit }
  | tv=typeVariable { tv }
  (*ArrayType*)

classOrInterfaceType:
  | ct=classType { ct }
  (*| it=interfaceType { it }*)

classType:
  | tds=typeDeclSpecifier { tds }
  (* | tds=typeDeclSpecifier tas=typeArguments { tds^" "^tas } *)

typeDeclSpecifier:
  | tn=typeName { tn }
  | coit=classOrInterfaceType POINT id=identifier { coit^"."^id }

typeName:
  | id=identifier { id }
  | tn=typeName POINT id=identifier {tn^"."^id}

typeVariable:
  | id=identifier { id }

arrayType: 
  | mt=myType LSBRAC RSBRAC { mt^"[]" }

numericType:
  | it=integralType { it }
  | fpt=floatingPointType { fpt }

integralType:
  | BYTE { "byte" } | SHORT { "short" } | INT { "int" } | LONG { "long" } | CHAR { "char" }

floatingPointType:
  | FLOAT { "float" } | DOUBLE { "double" }

resultType:
  | mt=myType { mt }
  | VOID { "void" }

myType:
  | pt = primitiveType { pt }
  (*| Identifier [TypeArguments]{ . Identifier [TypeArguments]} { [] } *)

basicType:
  | BYTE { "byte" } | SHORT { "short" } | INT { "int" } | LONG { "long" } | CHAR { "char" }
  | FLOAT { "float" } | DOUBLE { "double" }
  | BOOLEAN { "boolean" }
  



