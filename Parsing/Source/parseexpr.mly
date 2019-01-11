%{
  open Expr
%}

%token EOF PLUS MINUS TIMES DIV MOD LPAR RPAR SEMICOLON EQUAL
%token <int> INT
%token <string> IDENT
%token <float> FLOAT

%start expression
%type < Expr.expression > expression


%left PLUS MINUS
%left TIMES DIV MOD
%right UMINUS UPLUS

%%

expression:
 | e=expr EOF            { e }

expr:
  | LPAR e=expr RPAR
      { e }
  | MINUS e=expr %prec UMINUS
      { Unop(Uminus,e)}
  | PLUS e=expr %prec UPLUS
      { Unop(Uplus,e)}
  | e1=expr o=bop e2=expr
      { Binop(o,e1,e2)}
  | id=IDENT
      { Var id }
  | i=INT
      { Const i }

%inline bop:
  | MINUS     { Bsub }
  | PLUS      { Badd }
  | TIMES     { Bmul }
  | DIV       { Bdiv }
  | MOD       { Bmod }
  
%%