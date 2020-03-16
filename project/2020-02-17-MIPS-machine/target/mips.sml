structure MIPS = struct

val indent = ref 0
val bktab  = "\b\b\b\b" 

(* Colours*)
val rest = "\027[0m";
val blue = "\027[0;34m";        (* regs *)
val cyan = "\027[0;36m";        (* imm *)
val green = "\027[0;32m";       (* addr *)
val red = "\027[0;31m";         (* label *)
val yellow = "\027[0;33m";      (* instruction keywords *)

(* Datatypes *)
datatype ('l,'t) inst = LS of (ls * regs * addr)
					  | EXTR of ext
					  | COMAF of (conman * fre * real)
					  | COMAI of (conman * regs * int)
					  | ARLO2 of (arlo * regs * regs)
					  | ARLO3 of (arlo * regs * regs * src2)
					  | COMP of (compa * regs * regs * src2)
					  | BRJP1 of (brjp * string)
					  | BRJP2 of (brjp * regs * string)
					  | BRJP3 of (brjp * regs * src2 * string)
					  | DAMO1 of (dmov * regs)
					  | DAMO2 of (dmov * regs * regs)
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
 			  | Fregs of fre
and
		 fre = F0 | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12 
and
		 addr = READ of regs
		 	  | SYM of string 
			  | IMM of int
			  | IMRE of (int * regs)
			  | SYIM of (string * oper * int)
			  | SYIMRE of (string * oper * int * regs)
and			  
		 ls = LA | LB | LBU | LD | LH | LHU | LW | LWCZ | LWL | LWR 
			| SB | SD | SH | SW | SWCZ | SWL | SWR
			| ULH | ULHU | ULW | USH | USW
and
		 ext = RFE | SYSCALL | BREAK | NOP	
and
		 conman = LI | LID | LIS | LUI
and
		 oper = PL | MI	 
and
		 src2 = SRCINT of int
		 	  | SRCREG of regs
and
		 arlo = ABS | ADD | ADDI | ADDU | ADDIU | AND | ANDI | DIV | DIVU | MUL | MULO | MULOU
		      | MULT | MULTU | NEG | NEGU | NOR | NOT | OR | ORI | REM | REMU | ROL | ROR
		      | SLL | SLLV | SRA | SRAV | SRL | SRLV | SUB | SUBU | XOR | XORI
and
		 compa = SEQ | SGE | SGEU | SGT | SGTU | SLE | SLEU | SLT | SLTU | SLTI | SLTIU | SNE	      
and
		 brjp = B | BCZT | BCZF | BEQ | BEQZ | BGE | BGEU | BGEZ | BGT | BGTU | BGTZ
		 	  | BLE | BLEU | BLEZ | BGEZAL | BLTZAL | BLT | BLTU | BLTZ | BNE | BNEZ
		 	  | J | JAL | JALR | JR
and
		 dmov = MOVE | MFHI | MFLO | MTHI | MTLO | MFCZ | MFCD | MTCZ
		 
		 
(* Functions for converting Datatypes to strings *)
fun regsToString Zero = "$zero"
  | regsToString At = "$at"
  | regsToString V0 = "$v0"
  | regsToString V1 = "$v1"
  | regsToString A0 = "$a0"
  | regsToString A1 = "$a1"
  | regsToString A2 = "$a2"
  | regsToString A3 = "$a3" 
  | regsToString T0 = "$t0"
  | regsToString T1 = "$t1"
  | regsToString T2 = "$t2"
  | regsToString T3 = "$t3"
  | regsToString T4 = "$t4"
  | regsToString T5 = "$t5"
  | regsToString T6 = "$t6"
  | regsToString T7 = "$t7"
  | regsToString T8 = "$t8"
  | regsToString T9 = "$t9"
  | regsToString S0 = "$s0"
  | regsToString S1 = "$s1"
  | regsToString S2 = "$s2"
  | regsToString S3 = "$s3"
  | regsToString S4 = "$s4"
  | regsToString S5 = "$s5"
  | regsToString S6 = "$s6"
  | regsToString S7 = "$s7"
  | regsToString K0 = "$k0"
  | regsToString K1 = "$k1"
  | regsToString Gp = "$gp"
  | regsToString Sp = "$sp"
  | regsToString Fp = "$fp"
  | regsToString Ra = "$ra"
  | regsToString (Fregs f) = fregsToString f
  
and fregsToString F0 = "$f0"
  | fregsToString F1 = "$f1"
  | fregsToString F2 = "$f2"
  | fregsToString F3 = "$f3"
  | fregsToString F4 = "$f4"
  | fregsToString F5 = "$f5"
  | fregsToString F6 = "$f6"
  | fregsToString F7 = "$f7"
  | fregsToString F8 = "$f8"
  | fregsToString F9 = "$f9"
  | fregsToString F10 = "$f10"
  | fregsToString F11 = "$f11"
  | fregsToString F12 = "$f12"

fun operToString PL = "+"
  | operToString MI = "-"

fun addrToString (READ a) = "(" ^ (regsToString a) ^ ")"
  | addrToString (SYM a) = a
  | addrToString (IMM a) = Int.toString a
  | addrToString (IMRE (i,r)) = (Int.toString i) ^ (addrToString (READ r))
  | addrToString (SYIM (s,opr,i)) = s ^ " " ^ (operToString opr) ^ " " ^ (Int.toString i)
  | addrToString (SYIMRE (s,opr,i,r)) = s ^ " " ^ (operToString opr)^  " " ^ (addrToString (IMRE (i,r)))

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
					 
fun comaToString LI = "LI"
  | comaToString LID = "LI.D"
  | comaToString LIS = "LI.S"
  | comaToString LUI = "LUI"

fun arloToString ABS = "ABS"
  | arloToString ADD = "ADD"
  | arloToString ADDI = "ADDI"
  | arloToString ADDU = "ADDU"
  | arloToString ADDIU = "ADDIU"
  | arloToString AND = "AND"
  | arloToString ANDI = "ANDI"
  | arloToString DIV = "DIV"
  | arloToString DIVU = "DIVU"
  | arloToString MUL = "MUL"
  | arloToString MULO = "MULO"
  | arloToString MULOU = "MULOU"
  | arloToString MULT = "MULT"
  | arloToString MULTU = "MULTU"
  | arloToString NEG = "NEG"
  | arloToString NEGU = "NEGU"
  | arloToString NOR = "NOR"
  | arloToString NOT = "NOT"
  | arloToString OR = "OR"
  | arloToString ORI = "ORI"
  | arloToString REM = "REM"
  | arloToString REMU = "REMU"
  | arloToString ROL = "ROL"
  | arloToString ROR = "ROR"
  | arloToString SLL = "SLL"
  | arloToString SLLV = "SLLV" 
  | arloToString SRA = "SRA"
  | arloToString SRAV = "SRAV"
  | arloToString SRL = "SRL"
  | arloToString SRLV = "SRLV"
  | arloToString SUB = "SUB"
  | arloToString SUBU = "SUBU"
  | arloToString XOR = "XOR"
  | arloToString XORI = "XORI"

fun src2ToString (SRCREG r) = regsToString r
  | src2ToString (SRCINT i) = Int.toString i

fun compToString SEQ = "SEQ"
  | compToString SGE = "SGE"
  | compToString SGEU = "SGEU"
  | compToString SGT = "SGT"
  | compToString SGTU = "SGTU"
  | compToString SLE = "SLE"
  | compToString SLEU = "SLEU"
  | compToString SLT = "SLT"
  | compToString SLTU = "SLTU"
  | compToString SLTI = "SLTI"
  | compToString SLTIU = "SLTIU"
  | compToString SNE = "SNE"   

fun brjpToString B = "B"
  | brjpToString BCZT = "BCZT"
  | brjpToString BCZF = "BCZF"
  | brjpToString BEQ = "BEQ"
  | brjpToString BEQZ = "BEQZ"
  | brjpToString BGE = "BGE"
  | brjpToString BGEU = "BGEU"
  | brjpToString BGEZ = "BGEZ"
  | brjpToString BGT = "BGT"
  | brjpToString BGTU = "BGTU"
  | brjpToString BGTZ = "BGTZ"
  | brjpToString BLE = "BLE"
  | brjpToString BLEU = "BLEU"
  | brjpToString BLEZ = "BLEZ"
  | brjpToString BGEZAL = "BGEZAL" 
  | brjpToString BLTZAL = "BLTZAL"
  | brjpToString BLT = "BLT"
  | brjpToString BLTU = "BLTU"
  | brjpToString BLTZ = "BLTU"
  | brjpToString BNE = "BNE"
  | brjpToString BNEZ = "BNEZ"
  | brjpToString J = "J"
  | brjpToString JAL = "JAL" 
  | brjpToString JALR = "JALR" 
  | brjpToString JR = "JR"

fun dmovToString MOVE = "MOVE"
  | dmovToString MFHI = "MFHI"
  | dmovToString MFLO = "MFLO"
  | dmovToString MTHI = "MTHI"
  | dmovToString MTLO = "MTLO"
  | dmovToString MFCZ = "MFCZ"
  | dmovToString MFCD = "MFC1.D"
  | dmovToString MTCZ = "MTCZ"

(*
   Converts an instruction where the labels are strings and temps are
   actual machine registers to a string. This function is used at the
   end to emit the actual code.

   The pretty function has the type:

   val pretty : (string, regs) inst -> string

*)


fun pretty (LS (a,b,c)) = lsToString(a) ^ " " ^ regsToString(b) ^ " " ^  addrToString(c)
  | pretty (EXTR a) = extrToString(a)
  | pretty (COMAF (a,b,c)) = (comaToString a) ^ " " ^ (fregsToString b) ^ " " ^ (Real.toString c)
  | pretty (COMAI (a,b,c)) = (comaToString a) ^ " " ^ (regsToString b) ^ " " ^ (Int.toString c)
  | pretty (ARLO2 (a,r1,r2)) = (arloToString a) ^ " " ^ (regsToString r1) ^ " " ^ (regsToString r2)
  | pretty (ARLO3 (a,r1,r2,s)) = (pretty (ARLO2 (a,r1,r2))) ^ " " ^ (src2ToString s)
  | pretty (COMP (a,r1,r2,s)) = (compToString a) ^ " " ^ (regsToString r1) ^ " " ^ (regsToString r2) ^ " " ^ (src2ToString s)
  | pretty (BRJP1 (a,b)) = (brjpToString (a)) ^ " " ^ b
  | pretty (BRJP2 (a,b,c)) = (brjpToString (a)) ^ " " ^ (regsToString b) ^ " " ^ c
  | pretty (BRJP3 (a,b,c,d)) = (brjpToString (a)) ^ " " ^ (regsToString b) ^ " " ^ (src2ToString c) ^ " " ^ d
  | pretty (DAMO1 (a,b)) = (dmovToString a) ^ " " ^ (regsToString b)
  | pretty (DAMO2 (a,b,c)) = (pretty (DAMO1 (a,b))) ^ " " ^ (regsToString c)

fun prettyPrint (x::xs) = (pretty x) ^ "\n" ^ (prettyPrint xs)
  | prettyPrint [] = ""

end;

(* Main *)
val instr_list = [(MIPS.LS (MIPS.LA, MIPS.V0, MIPS.IMM 12)), 
				  (MIPS.EXTR MIPS.NOP), 
				  (MIPS.LS (MIPS.LB, MIPS.A1, MIPS.SYIMRE ("abc",MIPS.PL,9,MIPS.S0))),
				  (MIPS.COMAF (MIPS.LID, MIPS.F0, 10.01)),
				  (MIPS.ARLO3 (MIPS.OR, MIPS.T0, MIPS.T1, MIPS.SRCREG MIPS.T2)),
				  (MIPS.COMP (MIPS.SLT, MIPS.A1, MIPS.A2, MIPS.SRCINT 5)),
				  (MIPS.BRJP1 (MIPS.B, "def")),
				  (MIPS.DAMO1 (MIPS.MFHI, MIPS.T0))]

val _ = print ("\n\n====================================\n")
val _ = print (MIPS.prettyPrint instr_list) 
val _ = print ("====================================\n\n")

