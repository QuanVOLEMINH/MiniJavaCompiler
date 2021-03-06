%{
    open Expr
%}

(* Named by Java 1.6 Grammar Spec - Chapter 18 *)



(* Type *)
%token VOID

(* keywords *)
%token THIS SUPER EXTENDS CLASS BREAK RETURN DO CONTINUE WHILE IF ELSE
%token PUBLIC FINAL PROTECTED PRIVATE ABSTRACT STATIC STRICTFP

%token EOF
%token AT

%token BOOLEAN

(* Separators *)
%token SEMICOLON COMMA COLON POINT TILDE EP QM QuoM LPAR RPAR LBRAC RBRAC LSBRAC RSBRAC 

(* conditional operators *)
%token CONDITIONALAND CONDITIONALOR AND INCLUSIVEOR EXCLUSIVEOR CONDITIONALEQUAL CONDITIONALNOTEQUAL

(* bitwise operators *)
%token LSHIFT RSHIFT USHIFT

%token <string> IDENT
%token <int> INTEGER

(* Op *)
%token INCR DECR 

(* assignment operators *)
%token EQUAL TIMESEQUAL DIVEQUAL MODEQUAL PLUSEQUAL MINUSEQUAL LSHIFTQUAL RSHIFTQUAL USHIFTQUAL ANDQUAL EXCLUSIVEORQUAL INCLUSIVEORQUAL

(* boolean literal *)
%token TRUE FALSE

(* null literal *)
%token NULL

(* integralType *)
%token BYTE SHORT INT LONG CHAR

(* floatingPointType *)
%token FLOAT DOUBLE

(* InfixOp *)
%token PLUS MINUS TIMES DIV MOD LT GT LTOE GTOE

(* digits *)
%token ZERODIGIT
%token <char> NONZERODIGIT

%start compilationUnit
%type <string> compilationUnit

%nonassoc THENSTATE
%nonassoc ELSESTATE

%%

(* Identifier *)
identifier:
  | id=IDENT { id }

(* Modifiers *)
classModifiers:
  | cm=classModifier { cm }
  | cms=classModifiers cm=classModifier { cms^" "^cm }

classModifier:
  (*Annotation*)
  | PUBLIC { "public" } | PROTECTED { "protected" } | PRIVATE { "private" } | ABSTRACT { "abstract" } | STATIC { "static" } | FINAL { "final" } | STRICTFP { "strictfp" }

fieldModifiers:
  | fm=fieldModifier { fm }
  | fms=fieldModifiers fm=fieldModifier { fms^" "^fm }

fieldModifier:
  | PUBLIC { "public" }
  | PROTECTED { "protected" }
  | PRIVATE { "private" }
  | STATIC { "static" }
  | FINAL { "final" }
  (*Annotation   transient
  volatile*)

constructorModifiers:
  | cm=constructorModifier { cm }
  | cms=constructorModifiers cm=constructorModifier { cms^" "^cm }

constructorModifier:
  | PUBLIC {"public"}
  | PROTECTED {"protected"}
  | PRIVATE {"private"}
(*Annotation*)

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
  | cms=classModifiers CLASS id=identifier (*[TypeParameters] [extends Type] [implements TypeList]*) cb=classBody { cms^" class "^id^" "^cb }

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
  | swtbs=statementWithoutTrailingSubstatement  { swtbs }
  | ls=labeledStatement                         { ls }
  | IF LBRAC se=statementExpression RBRAC snif=statement ELSE st=statement  %prec ELSESTATE {("If "^se^" Then: "^snif^" Else: "^st)} 
  | IF LBRAC se=statementExpression RBRAC st=statement %prec THENSTATE {("If "^se^" Then:"^st)} 
  
(*
WhileStatement
ForStatement*)


statementWithoutTrailingSubstatement:
  | b=block { b }
  | es=emptyStatement { es }
  | es=expressionStatement { es }
  | brk= breakStatement { brk }
  | rtn= returnStatement { rtn }
  | cs = continueStatement { cs }
  (* AssertStatement *)
  (* SwitchStatement *)
  (* SynchronizedStatement *)
  (* ThrowStatement *)
  (* TryStatement *)

labeledStatement:
  | id=identifier COLON s=statement { id^":"^s }

emptyStatement:
  | SEMICOLON { ";" }

expressionStatement:
  | se=statementExpression SEMICOLON { se^";" }

continueStatement:
  | CONTINUE SEMICOLON { "continue \n" }
  | CONTINUE id=identifier SEMICOLON { ("continue "^id)}

doStatement: (* TODO: expression *)
  | DO blks=statement WHILE LPAR expr=expression RPAR SEMICOLON {("do " ^ blks^" while "^expr^"\n")}

returnStatement:
  | RETURN SEMICOLON { "return \n"}
  | RETURN expr= expression SEMICOLON { ("return \n"^expr)}

breakStatement:
  | BREAK SEMICOLON { "break \n"}
  | BREAK id= identifier SEMICOLON { ("break "^id)}

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

unaryExpression: (* ok *)
  | pie=preIncrementExpression { pie }
  | pde=preDecrementExpression { pde }
  | PLUS ue=unaryExpression { "+"^ue }
  | MINUS ue=unaryExpression { "-"^ue }
  | uenpm=unaryExpressionNotPlusMinus { uenpm }

unaryExpressionNotPlusMinus: (* ok *)
  | pe=postfixExpression { pe }
  | TILDE ue=unaryExpression { "~"^ue }
  | EP ue=unaryExpression { "!"^ue }
  | ce=castExpression { ce }

postfixExpression: (* ok *)
  | p=primary { p }
  | en=expressionName { en }
  | pie=postIncrementExpression { pie }
  | pde=postDecrementExpression { pde }

castExpression: 
  | LBRAC pt=primitiveType RBRAC ue=unaryExpression { "("^pt^")"^ue }
  | LBRAC rt=referenceType RBRAC uenpm=unaryExpressionNotPlusMinus { "("^rt^")"^uenpm }

constantExpression:
  | e=expression { e }

expression:
  | ae=assignmentExpression { ae }

assignmentExpression:
  | ce=conditionalExpression { ce }
  | a=assignment { a }

conditionalExpression:
  | coe=conditionalOrExpression { coe }
  | coe=conditionalOrExpression QM e=expression COLON ce=conditionalExpression { coe^"?"^e^":"^ce }

conditionalOrExpression:
  | cae=conditionalAndExpression { cae }
  | coe=conditionalOrExpression CONDITIONALOR cae=conditionalAndExpression { coe^"||"^cae }

conditionalAndExpression:
  | ioe=inclusiveOrExpression { ioe }
  | cae=conditionalAndExpression CONDITIONALAND ioe=inclusiveOrExpression { cae^"&&"^ioe }

inclusiveOrExpression:
  | eoe=exclusiveOrExpression { eoe }
  | ioe=inclusiveOrExpression INCLUSIVEOR eoe=exclusiveOrExpression { ioe^"|"^eoe }

exclusiveOrExpression:
  | ae=andExpression { ae }
  | eoe=exclusiveOrExpression EXCLUSIVEOR ae=andExpression { eoe^"^"^ae }

andExpression:
  | ee=equalityExpression { ee }
  | ae=andExpression AND ee=equalityExpression { ae^"&"^ee }

equalityExpression:
  | re=relationalExpression { re }
  | ee=equalityExpression CONDITIONALEQUAL re=relationalExpression { ee^"=="^re }
  | ee=equalityExpression CONDITIONALNOTEQUAL re=relationalExpression { ee^"!="^re }

relationalExpression:
  | se=shiftExpression { se }
  | re=relationalExpression LT se=shiftExpression { re^"<"^se }
  | re=relationalExpression GT se=shiftExpression { re^">"^se }
  | re=relationalExpression LTOE se=shiftExpression { re^"<="^se }
  | re=relationalExpression GTOE se=shiftExpression { re^">="^se }
  (*|instanceof ReferenceType*)

shiftExpression:
  | ae=additiveExpression { ae }
  | se=shiftExpression LSHIFT ae=additiveExpression { se^"<<"^ae }
  | se=shiftExpression RSHIFT ae=additiveExpression { se^">>"^ae }
  | se=shiftExpression USHIFT ae=additiveExpression { se^">>>"^ae }

additiveExpression:
  | me=multiplicativeExpression { me }
  | ae=additiveExpression PLUS me=multiplicativeExpression { ae^"+"^me }
  | ae=additiveExpression MINUS me=multiplicativeExpression { ae^"-"^me }

multiplicativeExpression:
  | ue=unaryExpression { ue }
  | me=multiplicativeExpression TIMES ue=unaryExpression { me^"*"^ue }
  | me=multiplicativeExpression DIV ue=unaryExpression { me^"/"^ue }
  | me=multiplicativeExpression MOD ue=unaryExpression { me^"%"^ue }

assignment:
  | lhs=leftHandSide ao=assignmentOperator ae=expression { lhs^" "^ao^" "^ae }

leftHandSide:
  | en=expressionName { en }
  | fa=fieldAccess { fa }
  (* arrayAccess*)

assignmentOperator:
  | EQUAL { "=" }
  | TIMESEQUAL { "*=" }
  | DIVEQUAL { "/=" }
  | MODEQUAL { "%=" }
  | PLUSEQUAL { "+=" }
  | MINUSEQUAL { "-=" }
  | LSHIFTQUAL { "<<=" }
  | RSHIFTQUAL { ">>=" }
  | USHIFTQUAL { ">>>=" }
  | ANDQUAL { "&="}
  | EXCLUSIVEORQUAL { "^=" }
  | INCLUSIVEORQUAL { "|=" }

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
  | il=integerLiteral { il }
  | fpl=floatingPointLiteral { fpl }
  | bl=booleanLiteral { bl }
(*CharacterLiteral*)
  | sl=stringLiteral { sl }
  | nl=nullLiteral { nl }

integerLiteral:
  | dil=decimalIntegerLiteral { dil }
(*HexIntegerLiteral
OctalIntegerLiteral*)

floatingPointLiteral:
  | dfpl=decimalFloatingPointLiteral { dfpl }
(*HexadecimalFloatingPointLiteral*)

booleanLiteral: (* ok *)
  | TRUE { "true" } | FALSE { "false" }

stringLiteral:
  | QuoM scs=stringCharacters QuoM { "\""^scs^"\"" }

nullLiteral:
  | NULL { "null" }

decimalIntegerLiteral:
  | dn=decimalNumeral { dn }
  (*| dn=decimalNumeral its=integerTypeSuffix { dn^" "^its }*)

decimalFloatingPointLiteral: (* review *)
  | ds=digits POINT { ds^"." }
  | ds1=digits POINT ds2=digits { ds1^"."^ds2 }
  | POINT ds=digits  { "."^ds }

decimalNumeral: (* ok *)
  | ZERODIGIT { "0" }
  | nzd=NONZERODIGIT { String.make 1 nzd }
  | nzd=NONZERODIGIT ds=digits { (String.make 1 nzd)^ds }

digits: (* ok *)
  | d=digit { d }
  | ds=digits d=digit { ds^d }

digit: (* ok *)
  | ZERODIGIT { "0" }
  | nzd=NONZERODIGIT { (String.make 1 nzd) } 

stringCharacters:
  | sc=stringCharacter { sc }
  | scs=stringCharacters sc=stringCharacter { scs^sc }

stringCharacter: (* to do *)
  | id=identifier { id }


fieldAccess: (* ok *)
  | p=primary POINT id=identifier { p^"."^id }
  | SUPER POINT id=identifier { "super."^id }
  | cn=className POINT SUPER POINT id=identifier { cn^".super."^id}

arrayAccess: (* ok *)
  | en=expressionName LSBRAC e=expression RSBRAC { en^"["^e^"]" }
  | pnna=primaryNoNewArray LSBRAC e=expression RSBRAC { pnna^"["^e^"]" }

className: (* review *)
  | id=identifier { id }

(* Type *)
primitiveType: (* ok *)
  | nt=numericType { nt }
  | BOOLEAN { "boolean" }

referenceType: (* ok *)
  | coit=classOrInterfaceType { coit }
  | tv=typeVariable { tv }
  | at=arrayType { at }

classOrInterfaceType: (* ok *)
  | ct=classType { ct }
  | it=interfaceType { it }

classType: (* ok *)
  | tds=typeDeclSpecifier { tds }
  | tds=typeDeclSpecifier tas=typeArguments { tds^" "^tas }

interfaceType: (* ok *)
  | tds=typeDeclSpecifier { tds }
  | tds=typeDeclSpecifier tas=typeArguments { tds^" "^tas }

typeDeclSpecifier: (* ok *)
  | tn=typeName { tn }
  | coit=classOrInterfaceType POINT id=identifier { coit^"."^id }

typeName: (* ok *)
  | id=identifier { id }
  | tn=typeName POINT id=identifier {tn^"."^id}

typeVariable: (* ok *)
  | id=identifier { id }

arrayType: (* ok *)
  | mt=myType LSBRAC RSBRAC { mt^"[]" }

typeArguments: (* ok *)
  | LT atal=actualTypeArgumentList GT { "<"^atal^">" }

actualTypeArgumentList: (* ok *)
  | ata=actualTypeArgument { ata }
  | atal=actualTypeArgumentList COMMA ata=actualTypeArgument { atal^","^ata }

actualTypeArgument: (* ok *)
  | rt=referenceType { rt }
  | wc=wildcard { wc }

wildcard: (* ok *)
  | QM { "?" }
  | QM wcbs=wildcardBounds { "?"^wcbs }

wildcardBounds: (* ok *)
  | EXTENDS rt=referenceType { "extends "^rt }
  | SUPER rt=referenceType { "super "^rt }

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
  



