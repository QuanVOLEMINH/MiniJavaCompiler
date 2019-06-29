let (accessModifiers: AST.modifier list) = 
  [AST.Private; AST.Public; AST.Protected]

let (nonAccessModifiers: AST.modifier list) = 
  [AST.Static; AST.Final; AST.Abstract; AST.Synchronized; AST.Transient; AST.Volatile; AST.Native]

let (allModifiers: AST.modifier list) = 
  accessModifiers@nonAccessModifiers

let (illegalPresentModifiers: AST.modifier list) = 
  [AST.Final; AST.Abstract]