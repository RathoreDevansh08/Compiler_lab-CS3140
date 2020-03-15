structure MIPS = struct

val indent = ref 0
val bktab  = "\b\b\b\b" 			   

datatype ('l,'t) inst = LS of (ls * regs * addr)
					  | EXTR of ext
and
		 regs = Zero 
			  | At
 			  | V0 | V1 
 			  | A0 | A1 | A2 | A3
 			  | T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 
 			  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7
 			  | K0 | K1
 			  | Gp | Sp | Fp
 			  | Ra
and
		 addr = STRAD of string 
			  | INTAD of int
and			  
		 ls = LA | LB | LBU | LD | LH | LHU | LW | LWCZ | LWL | LWR 
			| SB | SD | SH | SW | SWCZ | SWL | SWR
			| ULH | ULHU | ULW | USH | USW
and
		 ext = RFE | SYSCALL | BREAK | NOP		 

fun regsToString Zero = "zero"
  | regsToString At = "at"
  | regsToString V0 = "v0"
  | regsToString V1 = "v1"
  | regsToString A0 = "a0"
  | regsToString A1 = "a1"
  | regsToString A2 = "a2"
  | regsToString A3 = "a3" 
  | regsToString T0 = "t0"
  | regsToString T1 = "t1"
  | regsToString T2 = "t2"
  | regsToString T3 = "t3"
  | regsToString T4 = "t4"
  | regsToString T5 = "t5"
  | regsToString T6 = "t6"
  | regsToString T7 = "t7"
  | regsToString T8 = "t8"
  | regsToString T9 = "t9"
  | regsToString S0 = "s0"
  | regsToString S1 = "s1"
  | regsToString S2 = "s2"
  | regsToString S3 = "s3"
  | regsToString S4 = "s4"
  | regsToString S5 = "s5"
  | regsToString S6 = "s6"
  | regsToString S7 = "s7"
  | regsToString K0 = "k0"
  | regsToString K1 = "k1"
  | regsToString Gp = "gp"
  | regsToString Sp = "sp"
  | regsToString Fp = "fp"
  | regsToString Ra = "ra"

fun addrToString (STRAD a) = a
  | addrToString (INTAD a) = Int.toString a

fun lsToString LA = "LA"
  | lsToString LB = "LB"
  | lsToString LBU = "LBU"
  | lsToString LD = "LD"
  | lsToString LH = "LH"
  | lsToString LHU = "LHU"
  | lsToString LW = "LW"
  | lsToString LWCZ = "LWCZ"
  | lsToString LWL = "LWL"
  | lsToString LWR = "LWR"
  | lsToString SB = "SB"
  | lsToString SD = "SD"
  | lsToString SH = "SH"
  | lsToString SW = "SW"
  | lsToString SWCZ = "SWCZ"
  | lsToString SWL = "SWL"
  | lsToString SWR = "SWR"
  | lsToString ULH = "ULH"
  | lsToString ULHU = "ULHU"
  | lsToString ULW = "ULW"
  | lsToString USH = "USH"
  | lsToString USW = "USW"

fun extrToString RFE = "RFE"
  | extrToString SYSCALL = "SYSCALL"
  | extrToString BREAK = "BREAK"
  | extrToString NOP = "NOP"
					 

(*
   Converts an instruction where the labels are strings and temps are
   actual machine registers to a string. This function is used at the
   end to emit the actual code.

   The pretty function has the type:

   val pretty : (string, regs) inst -> string

*)


fun pretty (LS (a,b,c)) = lsToString(a) ^ " " ^ regsToString(b) ^ " " ^  addrToString(c)
  | pretty (EXTR a) = extrToString(a)

fun prettyPrint (x::xs) = (pretty x) ^ "\n" ^ (prettyPrint xs)
  | prettyPrint [] = "\n"

end;

val instr_list = [(MIPS.LS (MIPS.LA, MIPS.V0, MIPS.INTAD 12)), (MIPS.EXTR MIPS.NOP)]

val _ = print (MIPS.prettyPrint instr_list) 


