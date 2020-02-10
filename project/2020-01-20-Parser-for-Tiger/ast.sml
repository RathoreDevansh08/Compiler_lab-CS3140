structure Ast = struct

type ID = string

datatype BinOp = Plus
               | Minus
               | Mul
               | Div
               | Equals

datatype Expr  = INT of int
			   | ID of string
               | BINOP of Expr * BinOp * Expr
               | FUNC of (ID * Expr list)
               | IF of (Expr * Expr)
               | IFELSE of (Expr * Expr * Expr)
               | LET of (decl list * Expr list)
               | FOR of (ID * Expr * Expr * Expr)
               | WHILE of (Expr * Expr)
               | BREAK
and
		 decl = VARDECL of (ID * Expr)


fun binOpDenote Plus  x y = x + y
  | binOpDenote Minus x y = x - y
  | binOpDenote Mul   x y = x * y
  | binOpDenote Div   x y = floor(Real.fromInt(x) / Real.fromInt(y)) 
  | binOpDenote Equals   x y = if (x = y) then 1 else 0

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

fun plus  a b = BINOP (a, Plus, b)
fun minus a b = BINOP (a, Minus, b)
fun mul   a b = BINOP (a, Mul, b)
fun divi  a b = BINOP (a, Div, b)
fun equ	  a b = BINOP (a, Equals, b)

end

