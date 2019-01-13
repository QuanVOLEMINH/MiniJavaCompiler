%{
  open Expr
%}

(* Named by Java 1.6 Grammar Spec *)


(* AssignmentOperator *)
%token EQUAL

(* InfixOp *)
%token PLUS MINUS TIMES DIV MOD

(* Separators *)
%token LPAR RPAR SEMICOLON

(* Special *)
%token EOF

%token <int> INT
%token <string> IDENT

%start <string> compilationUnit
%%

(* Identifier *)
identifier:
    | id=IDENT { id }


(* Expression *)
expression:
    ae=assignmentExpression { ae }

(* AssignmentOperator *)
assignmentOperator:
    | EQUAL { "=" }

assignmentExpression:
    | a=assignment { a }

assignment:
    | lhs=leftHandSide ao=assignmentOperator id=identifier { lhs^" "^ao^" "^id }

leftHandSide:
    | en=expressionName { en }
    | fa=fieldAccess { fa }

expressionName:
    | id=identifier { id }

fieldAccess:
    | id=identifier { id }

compilationUnit:
    e=expression EOF {e}

%%