%{
  open Expr
%}

%token EOF  
%token PACKAGE IMPORT
%token SEMICOLON DOT TIMES
%token <string> IDENT

%start compilationunit
%type < Expr.compilationunit option> compilationunit


%%

compilationunit:
  | EOF                                 { None }
  | package     = packageunit SEMICOLON { Some (PackageDeclaration(package)) }  
  | importtype  = importUnit SEMICOLON  { Some (ImportDeclaration(importtype)) } 

(* PackageDeclaration *)
packageunit:
  | PACKAGE pkg=packagename                 { pkg }
packagename:
  | name  = IDENT                           { name }
  | name1 = IDENT DOT name2 = packagename   { name1^"."^name2 }

(* ImportDeclaration *)
importUnit:
  | IMPORT tname = typename                 { tname }
typename:
  | name = IDENT                            { name }
  | name1 = IDENT DOT name2 = suffixname    { name1^"."^name2 }
suffixname:
  | name = IDENT                            { name }
  | name = TIMES                            { "*" }
  | name1 = IDENT DOT name2 = suffixname    { name1^"."^name2 }

%%
