let (objectClass: AST.astclass) = 

  let obCl = {		
    AST.cid="Object";	
    AST.cname="Object";	
    AST.cscope = [];
    AST.cmodifiers = [AST.Public];
    AST.cparent = { tpath=[]; tid="Object" };
    AST.cattributes = [
      {
          amodifiers = [AST.Static];
          aname = "class";
          atype = Ref Type.class_type;
          adefault = None;
          aloc = Location.none;
      }
    ];
    AST.cinits = [];
    AST.cconsts = [];
    AST.cmethods = ObjectMethods.methods;
    AST.ctypes = [];	
    AST.cloc = Location.none;
  } in 
  obCl.cscope<-[obCl];
  obCl