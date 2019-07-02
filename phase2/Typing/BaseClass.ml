let (stringClass: AST.astclass) = 

  let sCl = {		
    AST.cid="String";	
    AST.cname="String";	
    AST.cscope = [];
    AST.cmodifiers = [AST.Public; AST.Final];
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
    AST.cconsts = [
      {
          cmodifiers = [AST.Public];
          cname = "String";
          cargstype = [];
          cthrows = [];
          cbody = [];
          mloc = Location.none;
      };
      {
        cmodifiers = [AST.Public];
        cname = "String";
        cargstype = [
          { 
            final = false; 
            vararg = false;
            ptype = Type.Ref {Type.tpath = []; Type.tid = "String"};
            pident = "original"
          }
        ];
        cthrows = [];
        cbody = [];
        mloc = Location.none;
      }
    ];
    AST.cmethods = [
      {
        mmodifiers = [AST.Public];
        mname = "hashCode";
        mreturntype = Primitive Type.Int;
        margstype = [];
        mthrows = [];
        mbody = [];
        mloc = Location.none;
        msemi = false;
      }
    ];
    AST.ctypes = [];	
    AST.cloc = Location.none;
  } in 
  sCl.cscope <-[sCl; Object.objectClass];
  sCl