(* The file tree.sml (See page 150 of the Modern Comp. Imp.  *)

signature TREE = sig
    type size
    type label = Temp.label

    datatype exp = CONST of LargeInt.int
             | NAME of Temp.label
             | TEMP of Temp.temp
             | BINOP of binop * exp * exp
             | MEM of exp
             | CALL of exp * exp list
             | ESEQ of stm * exp
         and stm = MOVE of exp * exp
             | EXP of exp
             | JUMP of exp * Temp.label list
             | CJUMP of relop * exp * exp * Temp.label * Temp.label
             | SEQ of stm * stm
             | LABEL of Temp.label
         and binop = PLUS | MINUS | MUL | DIV
             | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
         and relop = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE


    val notRel = relop -> relop
    val commute = relop -> relop
end

structure Tree : TREE = struct

    type size = int
    type label = Temp.label

    datatype exp = CONST of LargeInt.int
             | NAME of Temp.label
             | TEMP of Temp.temp
             | BINOP of binop * exp * exp
             | MEM of exp
             | CALL of exp * exp list
             | ESEQ of stm * exp
         and stm = MOVE of exp * exp
             | EXP of exp
             | JUMP of exp * Temp.label list
             | CJUMP of relop * exp * exp * Temp.label * Temp.label
             | SEQ of stm * stm
             | LABEL of Temp.label
         and binop = PLUS | MINUS | MUL | DIV
             | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
         and relop = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE



     fun notRel EQ = NE
       | notRel NE = EQ
       | notRel LT = GE
       | notRel GT = LE
       | notRel LE = GT
       | notRel GE = LT
       | notRel ULT = UGE
       | notRel ULE = UGT
       | notRel UGT = ULE
       | notRel UGE = ULT

     fun commute EQ = EQ
       | commute NE = NE
       | commute LT = GT
       | commute GE = LE
       | commute GT = LT
       | commute LE = GE
       | commute ULT = UGT
       | commute ULE = UGE
       | commute UGT = ULT
       | commute UGE = ULE

    (* helper functions for manipulating tree language *)
end
