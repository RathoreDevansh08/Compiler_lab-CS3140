functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "expr.grm"*)
(*#line 12.1 "expr.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
<<<<<<< HEAD
\\001\000\002\000\008\000\003\000\007\000\017\000\006\000\031\000\005\000\000\000\
\\001\000\003\000\030\000\000\000\
\\001\000\004\000\014\000\005\000\013\000\006\000\012\000\007\000\011\000\
\\010\000\010\000\024\000\009\000\000\000\
\\001\000\004\000\014\000\005\000\013\000\006\000\012\000\007\000\011\000\
\\010\000\010\000\032\000\027\000\000\000\
\\001\000\004\000\014\000\005\000\013\000\006\000\012\000\007\000\011\000\
\\018\000\057\000\019\000\057\000\023\000\057\000\024\000\057\000\
\\026\000\057\000\032\000\057\000\033\000\057\000\000\000\
\\001\000\011\000\037\000\000\000\
\\001\000\018\000\028\000\000\000\
\\001\000\019\000\041\000\000\000\
\\001\000\026\000\038\000\000\000\
\\048\000\000\000\
\\049\000\000\000\
\\050\000\002\000\008\000\003\000\007\000\017\000\006\000\031\000\005\000\000\000\
\\051\000\000\000\
\\052\000\025\000\020\000\000\000\
\\053\000\006\000\012\000\007\000\011\000\000\000\
\\054\000\006\000\012\000\007\000\011\000\000\000\
\\055\000\000\000\
\\056\000\000\000\
\\058\000\000\000\
\\059\000\000\000\
\\060\000\004\000\014\000\005\000\013\000\006\000\012\000\007\000\011\000\
\\010\000\010\000\033\000\040\000\000\000\
\\061\000\004\000\014\000\005\000\013\000\006\000\012\000\007\000\011\000\
\\010\000\010\000\000\000\
=======
\\001\000\002\000\011\000\003\000\010\000\017\000\009\000\031\000\008\000\
\\034\000\007\000\035\000\006\000\037\000\005\000\000\000\
\\001\000\003\000\018\000\000\000\
\\001\000\003\000\037\000\000\000\
\\001\000\004\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\010\000\013\000\024\000\012\000\000\000\
\\001\000\004\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\010\000\013\000\032\000\034\000\000\000\
\\001\000\004\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\010\000\013\000\036\000\049\000\000\000\
\\001\000\004\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\010\000\013\000\040\000\033\000\000\000\
\\001\000\004\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\010\000\013\000\040\000\058\000\000\000\
\\001\000\004\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\018\000\070\000\019\000\070\000\023\000\070\000\024\000\070\000\
\\026\000\070\000\032\000\070\000\033\000\070\000\036\000\070\000\
\\040\000\070\000\000\000\
\\001\000\011\000\032\000\000\000\
\\001\000\011\000\046\000\000\000\
\\001\000\018\000\035\000\000\000\
\\001\000\019\000\051\000\000\000\
\\001\000\026\000\047\000\000\000\
\\061\000\000\000\
>>>>>>> master
\\062\000\000\000\
\\063\000\002\000\011\000\003\000\010\000\017\000\009\000\031\000\008\000\
\\034\000\007\000\035\000\006\000\037\000\005\000\000\000\
\\064\000\000\000\
\\065\000\025\000\025\000\000\000\
\\066\000\006\000\015\000\007\000\014\000\000\000\
\\067\000\006\000\015\000\007\000\014\000\000\000\
\\068\000\000\000\
\\069\000\000\000\
\\071\000\000\000\
\\072\000\000\000\
\\073\000\004\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\010\000\013\000\033\000\050\000\000\000\
\\074\000\004\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\010\000\013\000\000\000\
\\075\000\004\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\010\000\013\000\000\000\
\\076\000\004\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\010\000\013\000\000\000\
\\077\000\000\000\
\\078\000\002\000\011\000\003\000\010\000\017\000\009\000\031\000\008\000\
\\034\000\007\000\035\000\006\000\037\000\005\000\000\000\
\\079\000\004\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\010\000\013\000\023\000\048\000\000\000\
\\080\000\000\000\
\\081\000\002\000\011\000\003\000\010\000\017\000\009\000\031\000\008\000\
\\034\000\007\000\035\000\006\000\037\000\005\000\000\000\
\\082\000\004\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\010\000\013\000\024\000\052\000\000\000\
\\083\000\000\000\
\\084\000\020\000\024\000\000\000\
\\085\000\024\000\036\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\004\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\010\000\013\000\000\000\
\"
val actionRowNumbers =
<<<<<<< HEAD
"\012\000\003\000\010\000\001\000\
\\031\000\014\000\013\000\012\000\
\\001\000\001\000\001\000\001\000\
\\001\000\004\000\032\000\007\000\
\\030\000\002\000\025\000\011\000\
\\005\000\018\000\017\000\016\000\
\\015\000\001\000\028\000\031\000\
\\006\000\009\000\024\000\021\000\
\\008\000\027\000\029\000\001\000\
\\020\000\025\000\001\000\019\000\
\\028\000\033\000\023\000\022\000\
\\026\000\000\000"
=======
"\017\000\004\000\015\000\030\000\
\\002\000\001\000\001\000\037\000\
\\019\000\018\000\017\000\001\000\
\\001\000\001\000\001\000\001\000\
\\010\000\007\000\005\000\040\000\
\\012\000\038\000\003\000\031\000\
\\016\000\009\000\023\000\022\000\
\\021\000\020\000\001\000\001\000\
\\001\000\034\000\037\000\011\000\
\\014\000\032\000\006\000\029\000\
\\026\000\013\000\035\000\039\000\
\\001\000\025\000\031\000\001\000\
\\001\000\024\000\034\000\041\000\
\\033\000\008\000\027\000\036\000\
\\001\000\028\000\000\000"
>>>>>>> master
val gotoT =
"\
\\001\000\058\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\017\000\000\000\
\\003\000\018\000\000\000\
\\005\000\021\000\006\000\020\000\007\000\019\000\000\000\
\\000\000\
\\000\000\
\\002\000\024\000\003\000\001\000\000\000\
\\003\000\025\000\000\000\
\\003\000\026\000\000\000\
\\003\000\027\000\000\000\
\\003\000\028\000\000\000\
\\003\000\029\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\037\000\008\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\038\000\000\000\
\\003\000\039\000\000\000\
\\003\000\040\000\000\000\
\\003\000\042\000\004\000\041\000\000\000\
\\005\000\021\000\006\000\043\000\007\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\051\000\000\000\
\\000\000\
\\003\000\037\000\008\000\052\000\000\000\
\\003\000\053\000\000\000\
\\003\000\054\000\000\000\
\\000\000\
\\003\000\042\000\004\000\055\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\057\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 59
val numrules = 28
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | ID of  (string) | INT of  (int) | EXPCOMM of  (Ast.Expr list) | VARDECL of  (Ast.decl) | DECS of  (Ast.decl list) | DEC of  (Ast.decl) | EXPS of  (Ast.Expr list) | EXP of  (Ast.Expr) | PROGRAM of  (Ast.Expr list) | S of  (Ast.Expr list)
end
type svalue = MlyValue.svalue
type result = Ast.Expr list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "INT"
  | (T 2) => "ID"
  | (T 3) => "PLUS"
  | (T 4) => "MINUS"
  | (T 5) => "MULTIPLY"
  | (T 6) => "DIVIDE"
  | (T 7) => "AND"
  | (T 8) => "OR"
  | (T 9) => "EQUALS"
  | (T 10) => "ASSIGN"
  | (T 11) => "NOTEQUAL"
  | (T 12) => "LESSTHAN"
  | (T 13) => "LESSTHANEQUAL"
  | (T 14) => "GRETHAN"
  | (T 15) => "GRETHANEQUAL"
  | (T 16) => "LET"
  | (T 17) => "IN"
  | (T 18) => "END"
  | (T 19) => "VAR"
  | (T 20) => "FUNCTION"
  | (T 21) => "COLON"
  | (T 22) => "COMMA"
  | (T 23) => "SEMICOLON"
  | (T 24) => "LPAREN"
  | (T 25) => "RPAREN"
  | (T 26) => "LSQBR"
  | (T 27) => "RSQBR"
  | (T 28) => "LCUBR"
  | (T 29) => "RCUBR"
  | (T 30) => "IF"
  | (T 31) => "THEN"
  | (T 32) => "ELSE"
  | (T 33) => "WHILE"
  | (T 34) => "FOR"
  | (T 35) => "TO"
  | (T 36) => "BREAK"
  | (T 37) => "TYPE"
  | (T 38) => "ARRAY"
  | (T 39) => "DO"
  | (T 40) => "OF"
  | (T 41) => "NIL"
  | (T 42) => "INTK"
  | (T 43) => "STRINGK"
  | (T 44) => "DOT"
  | (T 45) => "DOUBQUOTES"
  | (T 46) => "BACKSLASH"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
<<<<<<< HEAD
of  ( 0, ( ( _, ( MlyValue.PROGRAM PROGRAM, PROGRAM1left, PROGRAM1right)) :: rest671)) => let val  result = MlyValue.S ((*#line 51.19 "expr.grm"*)PROGRAM(*#line 267.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PROGRAM1left, PROGRAM1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.PROGRAM PROGRAM, _, PROGRAM1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 53.35 "expr.grm"*) EXP::PROGRAM (*#line 271.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, PROGRAM1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.PROGRAM ((*#line 54.22 "expr.grm"*) [] (*#line 275.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 56.39 "expr.grm"*) Ast.INT(INT) (*#line 279.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, INT1left, INT1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 57.17 "expr.grm"*) Ast.ID(ID) (*#line 283.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 58.39 "expr.grm"*) Ast.BINOP(EXP1, Ast.Plus, EXP2) (*#line 287.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 59.23 "expr.grm"*) Ast.BINOP(EXP1, Ast.Minus, EXP2) (*#line 291.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 60.25 "expr.grm"*) Ast.BINOP(EXP1, Ast.Mul, EXP2) (*#line 295.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 61.24 "expr.grm"*) Ast.BINOP(EXP1, Ast.Div, EXP2) (*#line 299.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 62.26 "expr.grm"*) Ast.BINOP(EXP1, Ast.Equals, EXP2) (*#line 303.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: _ :: ( _, ( MlyValue.DECS DECS, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 63.29 "expr.grm"*) Ast.LET(DECS, EXPS) (*#line 307.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, LET1left, END1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXPCOMM EXPCOMM, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 64.32 "expr.grm"*) Ast.FUNC(ID, EXPCOMM) (*#line 311.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 65.26 "expr.grm"*) Ast.IF(EXP1, EXP2) (*#line 315.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, IF1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 66.32 "expr.grm"*) Ast.IFELSE(EXP1, EXP2, EXP3) (*#line 319.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, IF1left, EXP3right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXPCOMM EXPCOMM, _, EXPCOMM1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPCOMM ((*#line 68.34 "expr.grm"*) EXP::EXPCOMM (*#line 323.1 "expr.grm.sml"*)
=======
of  ( 0, ( ( _, ( MlyValue.PROGRAM PROGRAM, PROGRAM1left, PROGRAM1right)) :: rest671)) => let val  result = MlyValue.S ((*#line 51.19 "expr.grm"*)PROGRAM(*#line 301.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PROGRAM1left, PROGRAM1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.PROGRAM PROGRAM, _, PROGRAM1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 53.35 "expr.grm"*) EXP::PROGRAM (*#line 305.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, PROGRAM1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.PROGRAM ((*#line 54.22 "expr.grm"*) [] (*#line 309.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 56.39 "expr.grm"*) Ast.INT(INT) (*#line 313.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, INT1left, INT1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 57.17 "expr.grm"*) Ast.ID(ID) (*#line 317.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 58.39 "expr.grm"*) Ast.BINOP(EXP1, Ast.Plus, EXP2) (*#line 321.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 59.23 "expr.grm"*) Ast.BINOP(EXP1, Ast.Minus, EXP2) (*#line 325.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 60.25 "expr.grm"*) Ast.BINOP(EXP1, Ast.Mul, EXP2) (*#line 329.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 61.24 "expr.grm"*) Ast.BINOP(EXP1, Ast.Div, EXP2) (*#line 333.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 62.26 "expr.grm"*) Ast.BINOP(EXP1, Ast.Equals, EXP2) (*#line 337.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: _ :: ( _, ( MlyValue.DECS DECS, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 63.29 "expr.grm"*) Ast.LET(DECS, EXPS) (*#line 341.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, LET1left, END1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXPCOMM EXPCOMM, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 64.32 "expr.grm"*) Ast.FUNC(ID, EXPCOMM) (*#line 345.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 65.26 "expr.grm"*) Ast.IF(EXP1, EXP2) (*#line 349.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, IF1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 66.32 "expr.grm"*) Ast.IFELSE(EXP1, EXP2, EXP3) (*#line 353.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, IF1left, EXP3right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 67.38 "expr.grm"*) Ast.FOR(ID, EXP1, EXP2, EXP3) (*#line 357.1 "expr.grm.sml"*)
>>>>>>> master
)
 in ( LrTable.NT 2, ( result, FOR1left, EXP3right), rest671)
end
<<<<<<< HEAD
|  ( 15, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.EXPCOMM ((*#line 69.17 "expr.grm"*) EXP::[] (*#line 327.1 "expr.grm.sml"*)
=======
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 68.24 "expr.grm"*) Ast.WHILE(EXP1, EXP2) (*#line 361.1 "expr.grm.sml"*)
>>>>>>> master
)
 in ( LrTable.NT 2, ( result, WHILE1left, EXP2right), rest671)
end
<<<<<<< HEAD
|  ( 16, ( rest671)) => let val  result = MlyValue.EXPCOMM ((*#line 70.23 "expr.grm"*) [] (*#line 331.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXPS EXPS, _, EXPS1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 72.32 "expr.grm"*) EXP::EXPS (*#line 335.1 "expr.grm.sml"*)
=======
|  ( 16, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 69.14 "expr.grm"*) Ast.BREAK (*#line 365.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 17, ( rest671)) => let val  result = MlyValue.EXPCOMM ((*#line 71.30 "expr.grm"*) [] (*#line 369.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.EXPCOMM ((*#line 72.17 "expr.grm"*) EXP::[] (*#line 373.1 "expr.grm.sml"*)
>>>>>>> master
)
 in ( LrTable.NT 7, ( result, EXP1left, EXP1right), rest671)
end
<<<<<<< HEAD
|  ( 18, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 73.17 "expr.grm"*) EXP::[] (*#line 339.1 "expr.grm.sml"*)
=======
|  ( 19, ( ( _, ( MlyValue.EXPCOMM EXPCOMM, _, EXPCOMM1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPCOMM ((*#line 73.31 "expr.grm"*) EXP::EXPCOMM (*#line 377.1 "expr.grm.sml"*)
>>>>>>> master
)
 in ( LrTable.NT 7, ( result, EXP1left, EXPCOMM1right), rest671)
end
<<<<<<< HEAD
|  ( 19, ( rest671)) => let val  result = MlyValue.EXPS ((*#line 74.23 "expr.grm"*) [] (*#line 343.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 20, ( ( _, ( MlyValue.DECS DECS, _, DECS1right)) :: _ :: ( _, ( MlyValue.DEC DEC, DEC1left, _)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 76.32 "expr.grm"*) DEC::DECS (*#line 347.1 "expr.grm.sml"*)
=======
|  ( 20, ( rest671)) => let val  result = MlyValue.EXPS ((*#line 77.27 "expr.grm"*) [] (*#line 381.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 78.18 "expr.grm"*) EXP::[] (*#line 385.1 "expr.grm.sml"*)
>>>>>>> master
)
 in ( LrTable.NT 3, ( result, EXP1left, EXP1right), rest671)
end
<<<<<<< HEAD
|  ( 21, ( ( _, ( MlyValue.DEC DEC, DEC1left, DEC1right)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 77.17 "expr.grm"*) DEC::[] (*#line 351.1 "expr.grm.sml"*)
=======
|  ( 22, ( ( _, ( MlyValue.EXPS EXPS, _, EXPS1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 79.27 "expr.grm"*) EXP::EXPS (*#line 389.1 "expr.grm.sml"*)
>>>>>>> master
)
 in ( LrTable.NT 3, ( result, EXP1left, EXPS1right), rest671)
end
<<<<<<< HEAD
|  ( 22, ( rest671)) => let val  result = MlyValue.DECS ((*#line 78.23 "expr.grm"*) [] (*#line 355.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 23, ( ( _, ( MlyValue.VARDECL VARDECL, VARDECL1left, VARDECL1right)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 80.24 "expr.grm"*) VARDECL (*#line 359.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, VARDECL1left, VARDECL1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.VARDECL ((*#line 82.34 "expr.grm"*) Ast.VARDECL(ID, EXP) (*#line 363.1 "expr.grm.sml"*)
=======
|  ( 23, ( rest671)) => let val  result = MlyValue.DECS ((*#line 83.28 "expr.grm"*) [] (*#line 393.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 24, ( ( _, ( MlyValue.DEC DEC, DEC1left, DEC1right)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 84.17 "expr.grm"*) DEC::[] (*#line 397.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, DEC1left, DEC1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.DECS DECS, _, DECS1right)) :: _ :: ( _, ( MlyValue.DEC DEC, DEC1left, _)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 85.27 "expr.grm"*) DEC::DECS (*#line 401.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, DEC1left, DECS1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.VARDECL VARDECL, VARDECL1left, VARDECL1right)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 88.24 "expr.grm"*) VARDECL (*#line 405.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, VARDECL1left, VARDECL1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.VARDECL ((*#line 90.34 "expr.grm"*) Ast.VARDECL(ID, EXP) (*#line 409.1 "expr.grm.sml"*)
>>>>>>> master
)
 in ( LrTable.NT 6, ( result, VAR1left, EXP1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.S x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.INT i,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.ID i,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun MULTIPLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun NOTEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun LESSTHANEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun GRETHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun GRETHANEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun LSQBR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun RSQBR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun LCUBR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun RCUBR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(ParserData.MlyValue.VOID,p1,p2))
fun INTK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(ParserData.MlyValue.VOID,p1,p2))
fun STRINGK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(ParserData.MlyValue.VOID,p1,p2))
fun DOUBQUOTES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(ParserData.MlyValue.VOID,p1,p2))
fun BACKSLASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(ParserData.MlyValue.VOID,p1,p2))
end
end
