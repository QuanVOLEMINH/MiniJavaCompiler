%{
    open Expr
%}

(* Named by Java 1.6 Grammar Spec - Chapter 18 *)

(* Modifier *)
%token PUBLIC FINAL

(* Type *)
%token VOID

(* Special *)
%token EOF
%token AT

(* Separators *)
%token SEMICOLON LPAR RPAR LBRAC RBRAC LSBRAC RSBRAC COMMA

%token <string> IDENT
%token <int> INT

%token CLASS BREAK RETURN DO

%start compilationUnit

%type <string> compilationUnit

(* PrefixOp *)
%token INCR

%token EQUAL

%token INTEGER

(* InfixOp *)
%token PLUS MINUS TIMES DIV MOD LT GT

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
  | m=modifier CLASS id=identifier (*[TypeParameters] [extends Type] [implements TypeList]*) cb=classBody { m^" class "^id^" "^cb }

classBody:
  | LPAR cbds=classBodyDeclarations RPAR { "{\n "^cbds^" \n}" }

classBodyDeclarations:
  | cbd=classBodyDeclaration { cbd }
  | cbds=classBodyDeclarations cbd=classBodyDeclaration {cbds^" "^cbd}

classBodyDeclaration:
  | cmd=classMemberDeclaration { cmd }
  | cd=constructorDeclaration { cd }

classMemberDeclaration:
  | fd=fieldDeclaration { fd }
  | md=methodDeclaration { md }
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
  | mh=methodHeader mb=methodBody { print_endline "methodDeclaration----------------";mh^" "^mb }

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
	| LPAR bss=blockStatements RPAR { " {\n"^bss^"\n}" }
	| LPAR RPAR { "{}\n"}

blockStatements:
  | bs=blockstatement { bs }
  | blockStatements bs=blockstatement { bs }

blockstatement:
  | lvds=localVariableDeclarationStatement { lvds }
  | swts=statementWithoutTrailingSubstatement { swts }

statementWithoutTrailingSubstatement:
  | b=block                       {b}
  | SEMICOLON                     {";\n"}
  | brk= breakStatement           { brk }
  | rtn= returnStatement          { rtn }
  | expr= expression SEMICOLON    { expr } (* TODO: expression*)
  | ds = doStatement              { ds }
  | cs = continueStatement        { cs }
  (* AssertStatement *)
  (* SwitchStatement *)
  (* SynchronizedStatement *)
  (* ThrowStatement *)
  (* TryStatement *)

continueStatement:
  | continue SEMICOLON { "continue \n" }
  | continue id=identifier SEMICOLON { ("continue "^id)}

doStatement: (* TODO: expression *)
  | DO blks = blockstatement WHILE LPAR expr=expression RPAR SEMICOLON {("do " ^ blks^" while "^expr^"\n")}


returnStatement:
  | RETURN SEMICOLON { "return \n"}
  | RETURN expr= expression SEMICOLON { ("return \n"^expr)}

breakStatement:
  | BREAK SEMICOLON { "break \n"}
  | BREAK id= identifier SEMICOLON { ("break "^id)}


localVariableDeclarationStatement:
  | (*[final]*) mt=myType vds=variableDeclarators SEMICOLON{ mt^" "^vds }

resultType:
  | mt=myType { mt }
  | VOID { "void" }

myType:
  | b=basicType { b }
  (*| Identifier [TypeArguments]{ . Identifier [TypeArguments]} { [] } *)

basicType:
  | INTEGER {print_endline "int -----------------";"int"}
  
expression:
  | ae=assignmentExpression { ae }

assignmentExpression:
  | a=assignment { a }

assignment:
  | lhs=leftHandSide ao=assignmentOperator ae=expression { lhs^" "^ao^" "^ae }

leftHandSide:
  | en=expressionName { en }
  (* fieldAccess
  arrayAccess*)

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
  (*| AmbiguousName . Identifier *)


