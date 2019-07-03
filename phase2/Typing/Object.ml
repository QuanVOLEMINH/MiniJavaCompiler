let (objectClass: AST.astclass list) = 

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

  let cl = {
    AST.cid="Class";
    AST.cname="Class";
    AST.cscope=[];
    AST.cmodifiers=[Public];
    AST.cparent = {tpath=[]; tid="Object"} ;
    AST.cattributes = [];
    AST.cinits = [];
    AST.cconsts = [];
    AST.cmethods = [];
    AST.ctypes = [];
    AST.cloc = Location.none;
    } in
   
  obCl.cscope <-[obCl;cl]; cl.cscope <- [obCl; cl];

  [obCl; cl]