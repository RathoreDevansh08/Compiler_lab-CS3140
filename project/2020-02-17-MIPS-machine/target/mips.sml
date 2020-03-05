structure MIPS = struct

datatype regs = zero 
			  | at
 			  | v0 | v1 
 			  | a0 | a1 | a2 | a3
 			  | t0 | t1 | t2 | t3 | t4 | t5 | t6 | t7 | t8 | t9 
 			  | s0 | s1 | s2 | s3 | s4 | s5 | s6 | s7
 			  | k0 | k1
 			  | gp | sp | fp
 			  | ra

val indent = ref 0
val bktab  = "\b\b\b\b" 			   

datatype addr = STRAD of string | INTAD of int

datatype ls = LA | LB | LBU | LD | LH | LHU | LW | LWCZ | LWL | LWR 
			| SB | SD | SH | SW | SWCZ | SWL | SWR
			| ULH | ULHU | ULW | USH | USW

datatype ('l,'t) inst = LS of ls * 't * addr
	|

(*
   Converts an instruction where the labels are strings and temps are
   actual machine registers to a string. This function is used at the
   end to emit the actual code.

   The pretty function has the type:

   val pretty : (string, regs) inst -> string

*)


fun pretty i = ...

end

val instr_list = []
