let (methods: AST.astmethod list) = 
  let (mClone: AST.astmethod) = {
    AST.mmodifiers = [Protected];
    AST.mname = "clone";
    AST.mreturntype = Ref Type.object_type;
    AST.margstype = [];
    AST.mthrows = [{ Type.tpath=[]; Type.tid="CloneNotSupportedException" }];
    AST.mbody = [];
    AST.mloc = Location.none;
    AST.msemi = false;
  } in

  let (mEquals: AST.astmethod) = {
    AST.mmodifiers = [Public];
    AST.mname = "equals";
    AST.mreturntype = Primitive Type.Boolean;
    AST.margstype = [ 
      {
          final = false;
          vararg = false;
          ptype = Ref Type.object_type;
          pident = "obj";
      }
    ];
    AST.mthrows = [];
    AST.mbody = [];
    AST.mloc = Location.none;
    AST.msemi = false;
  } in

  let (mFinalize: AST.astmethod) = {
    AST.mmodifiers = [Protected];
    AST.mname = "finalize";
    AST.mreturntype = Type.Void;
    AST.margstype = [];
    AST.mthrows = [{Type.tpath=[];Type.tid="Throwable"}];
    AST.mbody = [];
    AST.mloc = Location.none;
    AST.msemi = false;
  } in
  
  let (mHashCode: AST.astmethod) = {
    AST.mmodifiers = [Public];
    AST.mname = "hashCode";
    AST.mreturntype = Primitive Type.Int;
    AST.margstype = [];
    AST.mthrows = [];
    AST.mbody = [];
    AST.mloc = Location.none;
    AST.msemi = false;
  } in

  let (mNotify: AST.astmethod) = {
    AST.mmodifiers = [Public; Final];
    AST.mname = "notify";
    AST.mreturntype = Type.Void;
    AST.margstype = [];
    AST.mthrows = [];
    AST.mbody = [];
    AST.mloc = Location.none;
    AST.msemi = false;
  } in

  let (mNotifyAll: AST.astmethod) = {
      AST.mmodifiers = [Public; Final];
      AST.mname = "notifyAll";
      AST.mreturntype = Type.Void;
      AST.margstype = [];
      AST.mthrows = [];
      AST.mbody = [];
      AST.mloc = Location.none;
      AST.msemi = false;
  } in

  let (mToString: AST.astmethod) = {
    AST.mmodifiers = [Public];
    AST.mname = "toString";
    AST.mreturntype = Ref { Type.tpath=[]; Type.tid="String" };
    AST.margstype = [];
    AST.mthrows = [];
    AST.mbody = [];
    AST.mloc = Location.none;
    AST.msemi = false;
  } in

  let (mWait1: AST.astmethod) =  {
    AST.mmodifiers = [Public; Final];
    AST.mname = "wait";
    AST.mreturntype = Type.Void;
    AST.margstype = [];
    AST.mthrows = [{ Type.tpath=[]; Type.tid="InterruptedException" }];
    AST.mbody = [];
    AST.mloc = Location.none;
    AST.msemi = false;
  } in

  let (mWait2: AST.astmethod) =  {
    AST.mmodifiers = [Public; Final];
    AST.mname = "wait";
    AST.mreturntype = Type.Void;
    AST.margstype = [
        {
            final = false;
            vararg = false;
            ptype = Primitive Type.Long;
            pident = "timeout";
        }
    ];
    AST.mthrows = [{ Type.tpath=[]; Type.tid="InterruptedException" }];
    AST.mbody = [];
    AST.mloc = Location.none;
    AST.msemi = false;
  } in

  let (mWait3: AST.astmethod) = {
    AST.mmodifiers = [Public; Final];
    AST.mname = "wait";
    AST.mreturntype = Type.Void;
    AST.margstype = [
        {
            final = false;
            vararg = false;
            ptype = Primitive Type.Long;
            pident = "timeout";
        };
        {
            final = false;
            vararg = false;
            ptype = Primitive Type.Int;
            pident = "nanos";
        };
    ];
    AST.mthrows = [{ Type.tpath=[]; Type.tid="InterruptedException" }];
    AST.mbody = [];
    AST.mloc = Location.none;
    AST.msemi = false;
  } in

  [
    mClone; mEquals; mFinalize; mHashCode; mNotify; mNotifyAll; mToString; mWait1; mWait2; mWait3;
  ]