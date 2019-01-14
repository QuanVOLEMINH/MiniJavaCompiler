%{
    open Expr
%}

(* InfixOp *)
%token PLUS MINUS TIMES DIV MOD

(* PrefixOp *)
%token INCR

%token EQUAL

%token INTEGER

%token <float> FLOAT
%token <int> INT
%token <string> STRING

%start expression
%type <Expr.expression> expression

%%

(* Expression *)

expression:
  | ae=assignmentExpression { ae }

assignmentExpression:
  | a=assignment { a }

assignment:
  | lhs=leftHandSide ao=assignmentOperator ae=expression { Assign(lhs, ao, ae) }

leftHandSide:
  | en=expressionName { en }

expressionName:
  | id=identifier { Var id }

assignmentOperator:
  | EQUAL { Equal }

prefixOperator:
  | INCR { "++" }

%public
myType:
  | b=basicType { b }

basicType:
  | INTEGER {"int"}

%inline assign:
  | EQUAL         { Equal }

%%
