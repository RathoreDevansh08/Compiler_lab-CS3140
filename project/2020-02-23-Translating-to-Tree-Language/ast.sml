structure Ast = struct

type ID = string

datatype BinOp = Plus
               | Minus
               | Mul
               | Div
               | Equals
               | NotEqual
               | LessThan
               | LessThanEqual
               | GreThan
               | GreThanEqual

datatype Prog = PE of Expr
			   | PD of (decl list )
and
		 Expr  = INT of int
			   | ID of string
			   | ARR of Ty * Expr * Expr
			   | NEW of Ty
			   | LVE of Lvalue
               | BINOP of Expr * BinOp * Expr
               | FUNC of (ID * Expr list)
               | FUNCALL of (Lvalue * Expr)
               | BREX of (Expr list)
               | NEXP of (Expr)
               | IF of (Expr * Expr)
               | IFELSE of (Expr * Expr * Expr)
               | LET of (decl list * Expr list)
               | FOR of (ID * Expr * Expr * Expr)
               | WHILE of (Expr * Expr)
               | ASSIGN of (Lvalue * Expr)
               | BREAK
and
		 Decls = EMPTYDEC
		       | SINDEC of decl
		       | MULDEC of (decl list)
and
		 decl = VARDECL of (ID * Expr)
		 	  | TYPEDEC of (ID * Ty)
		 	  | IMPDEC  of (ID)
and
		 Lvalue  = LVAL_IDEN of ID
            | FIELD of (Lvalue * ID)
            | ELEMENT of (Lvalue * Expr)		 
and
		 Ty = TYPEID of ID

fun binOpDenote Plus  x y = x + y
  | binOpDenote Minus x y = x - y
  | binOpDenote Mul   x y = x * y
  | binOpDenote Div   x y = floor(Real.fromInt(x) / Real.fromInt(y)) 
  | binOpDenote Equals   x y = if (x = y) then 1 else 0
  | binOpDenote NotEqual   x y = if (x = y) then 0 else 1
  | binOpDenote LessThan   x y = if (x < y) then 1 else 0
  | binOpDenote LessThanEqual   x y = if (x <= y) then 1 else 0
  | binOpDenote GreThan   x y = if (x > y) then 1 else 0
  | binOpDenote GreThanEqual   x y = if (x >= y) then 1 else 0
	
(*
fun exprDenote (INT x) = x
  | exprDenote (ID x) = x
  | exprDenote (BINOP (x,oper,y)) = binOpDenote oper (exprDenote x) (exprDenote y)
*)

fun binOpToString Plus     = "+"
  | binOpToString Minus    = "-"
  | binOpToString Mul      = "*"
  | binOpToString Div      = "/"
  | binOpToString Equals   = "="
  | binOpToString NotEqual   = "<>"
  | binOpToString LessThan   = "<"
  | binOpToString LessThanEqual   = "<="
  | binOpToString GreThan   = ">"
  | binOpToString GreThanEqual   = ">="

fun plus  a b = BINOP (a, Plus, b)
fun minus a b = BINOP (a, Minus, b)
fun mul   a b = BINOP (a, Mul, b)
fun divi  a b = BINOP (a, Div, b)
fun equ	  a b = BINOP (a, Equals, b)
fun notequ	  a b = BINOP (a, NotEqual, b)
fun lessthan	  a b = BINOP (a, LessThan, b)
fun lessthanequ	  a b = BINOP (a, LessThanEqual, b)
fun grethan	  a b = BINOP (a, GreThan, b)
fun grethanequ	  a b = BINOP (a, GreThanEqual, b)


end

