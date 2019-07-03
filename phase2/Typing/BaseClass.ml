let (baseClass: AST.astclass list) = 

  let sCl = {		
    AST.cid="String";	
    AST.cname="String";	
    AST.cscope = Object.objectClass;
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
  
  let f = {
    AST.cid="Float";
    AST.cname="Float";
    AST.cscope=Object.objectClass;
    AST.cmodifiers=[Public];
    AST.cparent = {tpath=[]; tid="Object"} ;
    AST.cattributes = [];
    AST.cinits = [];
    AST.cconsts = [];
    AST.cmethods = [];
    AST.ctypes = [];
    AST.cloc = Location.none;
  } in
  
  let b = {
    AST.cid="Boolean";
    AST.cname="Boolean";
    AST.cscope=Object.objectClass;
    AST.cmodifiers=[Public];
    AST.cparent = {tpath=[]; tid="Object"} ;
    AST.cattributes = [];
    AST.cinits = [];
    AST.cconsts = [];
    AST.cmethods = [];
    AST.ctypes = [];
    AST.cloc = Location.none;
  } in

  let e = {
    AST.cid="Exception";
    AST.cname="Exception";
    AST.cscope=Object.objectClass;
    AST.cmodifiers=[Public];
    AST.cparent = {tpath=[]; tid="Object"} ;
    AST.cattributes = [];
    AST.cinits = [];
    AST.cconsts = [
         {
            cmodifiers = [];
            cname = "Exception";
            cargstype = [];
            cthrows = [];
            cbody = [];
            mloc = Location.none;
        };
        {
            cmodifiers = [];
            cname = "Exception";
            cargstype = [
                {
                    final = false;
                    vararg = false;
                    ptype = Type.Ref ({Type.tpath=[];Type.tid="String"});
                    pident = "str";
                }
            ];
            cthrows = [];
            cbody = [];
            mloc = Location.none;
        }

    ];
    AST.cmethods = [];
    AST.ctypes = [];
    AST.cloc = Location.none;
  } in

  [sCl; f; e; b]