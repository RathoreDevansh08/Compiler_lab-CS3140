# Translating to the Tree Language.

**NOTE:**This weeks assignment has only the project component and
hence all code that you write as part of this assignment should go to
your project subdirectory of your repository.

The text book defines the "Tree Language" which is given below serves
as an intermediate language to target the tiger compiler to.

```
(* The file tree.sml (See page 150 of the Modern Comp. Imp.  *)

structure TREE = struct

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

	 (* helper functions for manipulating tree language *)
end

```

The goal in this exercise is to translate tiger language to the TREE
language. The book suggests a structure `Translate` for doing the
translation of tiger expressions (remember tiger language is just a
single expression) to the Tree expression. You can make the following
simplifying assumption.


1. Assume that the only type available is `int`. You can differ
   support for strings and array for a later version. As a result you
   have trivial type checking.

2. Get a working version with no function calls. Then implement
   functions.

3. While implementing functions start with assuming that all
   parameters and locals are allocated on the stack. When ever you
   want to do operations on the variables, you will bring it to the
   register and work on it.  The k-th variables can then be accessed via the
   expression `MEM (BINOP (PLUS, TEMP fp, CONST k))`. This operation
   is supposed to capture the k-th word in the machine.

   Note that the code generated as a result is rather inefficient.  In
   a later stage you can implement one of the register allocation
   algorithms for this sorry state of affairs to be fixed.

The modules that you will need to implement for this step are the following.

1. The structure `Tree` defined in `tree.sml`

2. The structure `Temp` defined in `temp.sml`. This module is defined
   to take care of temporary variables and label creation.

3. For function calls you will also need the `Frame` module. See 6.2
   (page 134).
