structure TREE = struct

datatype exp = Ex of Tree.exp
			 | Nx of Tree.stm
			 | Cx of Temp.label * Temp.label -> Tree.stm
(*
Eg. Tiger expression a>b | c<d might translate to conditional:
Cx( fn (t,f) => SEQ(CJUMP(GT,a,b,t,z), SEQ(LABEL z, CJUMP(LT,c,d,t,f))))
*)

end
