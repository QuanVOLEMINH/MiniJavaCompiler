let (logicOperators: AST.infix_op list) = 
  [AST.Op_cor; AST.Op_cand; AST.Op_or; AST.Op_and; AST.Op_xor; AST.Op_eq; AST.Op_ne]

let (compareOperators: AST.infix_op list) = 
  [AST.Op_eq; AST.Op_ne; AST.Op_gt; AST.Op_lt; AST.Op_ge; AST.Op_le]

let calculOperators: AST.infix_op list = 
  [AST.Op_shl; AST.Op_shr; AST.Op_shrr; AST.Op_add; AST.Op_sub; AST.Op_mul; AST.Op_div; AST.Op_mod]