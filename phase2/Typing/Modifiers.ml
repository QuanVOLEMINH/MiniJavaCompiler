let (accessModifiers: AST.modifier list) = 
  [AST.Private; AST.Public; AST.Protected]

let (classNonAccessModifiers: AST.modifier list) = 
  [AST.Static; AST.Final; AST.Abstract; AST.Strictfp]
 
let (classModifiers: AST.modifier list) = 
  accessModifiers@classNonAccessModifiers

let (methodNonAccessModifiers: AST.modifier list) = 
  [AST.Static; AST.Final; AST.Abstract; AST.Synchronized; AST.Native; AST.Strictfp]

let (methodModifiers: AST.modifier list) = 
  accessModifiers@methodNonAccessModifiers 

let (illegalModifierCombiFA: AST.modifier list) = 
  [AST.Final; AST.Abstract]

let (illegalModifierCombiNS: AST.modifier list) = 
  [AST.Native; AST.Strictfp]

let (illegalModifierCombiPA: AST.modifier list) = 
  [AST.Private; AST.Abstract]

let (illegalModifierCombiSA: AST.modifier list) = 
  [AST.Static; AST.Abstract]
