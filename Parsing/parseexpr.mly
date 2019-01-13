%{
  open Expr
%}

%token EOF SEMICOLON PACKAGE IMPORT 
%token <string> IDENT

%start compilationunit
%type < Expr.compilationunit option> compilationunit


%%

compilationunit:
  | EOF                       { None }
  | pu= packageunit SEMICOLON { Some (PackageUnit("package", pu)) }  
  | iu= importUnit SEMICOLON  { Some (ImportUnit("import", iu)) } 

packageunit:
  | PACKAGE id=IDENT { id }
importUnit:
  | IMPORT id=IDENT { id }

%%
