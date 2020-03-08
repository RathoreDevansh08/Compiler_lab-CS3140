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
\\001\000\001\000\085\000\004\000\085\000\005\000\085\000\006\000\085\000\
\\007\000\085\000\010\000\085\000\011\000\118\000\012\000\085\000\
\\013\000\085\000\014\000\085\000\015\000\085\000\016\000\085\000\
\\018\000\085\000\019\000\085\000\023\000\085\000\024\000\085\000\
\\025\000\038\000\026\000\085\000\032\000\085\000\033\000\085\000\
\\036\000\085\000\040\000\085\000\045\000\118\000\000\000\
\\001\000\001\000\090\000\004\000\031\000\005\000\030\000\006\000\029\000\
\\007\000\028\000\018\000\090\000\019\000\090\000\023\000\090\000\
\\024\000\090\000\026\000\090\000\032\000\090\000\033\000\090\000\
\\036\000\090\000\040\000\090\000\000\000\
\\001\000\001\000\091\000\004\000\031\000\005\000\030\000\006\000\029\000\
\\007\000\028\000\018\000\091\000\019\000\091\000\023\000\091\000\
\\024\000\091\000\026\000\091\000\032\000\091\000\033\000\091\000\
\\036\000\091\000\040\000\091\000\000\000\
\\001\000\001\000\092\000\004\000\031\000\005\000\030\000\006\000\029\000\
\\007\000\028\000\018\000\092\000\019\000\092\000\023\000\092\000\
\\024\000\092\000\026\000\092\000\032\000\092\000\033\000\092\000\
\\036\000\092\000\040\000\092\000\000\000\
\\001\000\001\000\093\000\004\000\031\000\005\000\030\000\006\000\029\000\
\\007\000\028\000\018\000\093\000\019\000\093\000\023\000\093\000\
\\024\000\093\000\026\000\093\000\032\000\093\000\033\000\093\000\
\\036\000\093\000\040\000\093\000\000\000\
\\001\000\001\000\094\000\004\000\031\000\005\000\030\000\006\000\029\000\
\\007\000\028\000\018\000\094\000\019\000\094\000\023\000\094\000\
\\024\000\094\000\026\000\094\000\032\000\094\000\033\000\094\000\
\\036\000\094\000\040\000\094\000\000\000\
\\001\000\001\000\095\000\004\000\031\000\005\000\030\000\006\000\029\000\
\\007\000\028\000\018\000\095\000\019\000\095\000\023\000\095\000\
\\024\000\095\000\026\000\095\000\032\000\095\000\033\000\095\000\
\\036\000\095\000\040\000\095\000\000\000\
\\001\000\002\000\017\000\003\000\016\000\017\000\015\000\025\000\013\000\
\\031\000\012\000\034\000\011\000\035\000\010\000\037\000\009\000\000\000\
\\001\000\003\000\032\000\000\000\
\\001\000\003\000\036\000\000\000\
\\001\000\003\000\040\000\000\000\
\\001\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\026\000\056\000\000\000\
\\001\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\026\000\061\000\000\000\
\\001\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\032\000\055\000\000\000\
\\001\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\036\000\070\000\000\000\
\\001\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\040\000\054\000\000\000\
\\001\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\040\000\078\000\000\000\
\\001\000\011\000\053\000\000\000\
\\001\000\011\000\057\000\000\000\
\\001\000\018\000\058\000\000\000\
\\001\000\019\000\072\000\000\000\
\\001\000\026\000\068\000\000\000\
\\081\000\000\000\
\\082\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\086\000\006\000\029\000\007\000\028\000\000\000\
\\087\000\006\000\029\000\007\000\028\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\096\000\000\000\
\\097\000\011\000\021\000\025\000\020\000\045\000\019\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\033\000\071\000\000\000\
\\102\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\000\000\
\\103\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\000\000\
\\104\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\000\000\
\\105\000\000\000\
\\106\000\002\000\017\000\003\000\016\000\017\000\015\000\025\000\013\000\
\\031\000\012\000\034\000\011\000\035\000\010\000\037\000\009\000\000\000\
\\107\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\023\000\069\000\000\000\
\\108\000\000\000\
\\109\000\002\000\017\000\003\000\016\000\017\000\015\000\025\000\013\000\
\\031\000\012\000\034\000\011\000\035\000\010\000\037\000\009\000\000\000\
\\110\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\024\000\073\000\000\000\
\\111\000\000\000\
\\112\000\002\000\017\000\003\000\016\000\017\000\015\000\020\000\014\000\
\\025\000\013\000\031\000\012\000\034\000\011\000\035\000\010\000\
\\037\000\009\000\000\000\
\\112\000\020\000\014\000\000\000\
\\113\000\024\000\018\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\117\000\004\000\031\000\005\000\030\000\006\000\029\000\007\000\028\000\
\\010\000\027\000\012\000\026\000\013\000\025\000\014\000\024\000\
\\015\000\023\000\016\000\022\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\"
val actionRowNumbers =
"\047\000\051\000\025\000\049\000\
\\032\000\024\000\023\000\040\000\
\\009\000\008\000\008\000\008\000\
\\010\000\048\000\001\000\026\000\
\\048\000\011\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\018\000\016\000\
\\014\000\012\000\019\000\020\000\
\\041\000\050\000\053\000\013\000\
\\033\000\007\000\006\000\005\000\
\\004\000\003\000\002\000\030\000\
\\029\000\028\000\027\000\008\000\
\\008\000\008\000\031\000\008\000\
\\044\000\022\000\042\000\054\000\
\\015\000\039\000\036\000\052\000\
\\021\000\045\000\035\000\041\000\
\\008\000\008\000\034\000\044\000\
\\043\000\017\000\037\000\046\000\
\\008\000\038\000\000\000"
val gotoT =
"\
\\001\000\078\000\002\000\006\000\003\000\005\000\005\000\004\000\
\\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\031\000\005\000\004\000\000\000\
\\003\000\032\000\005\000\004\000\000\000\
\\003\000\033\000\005\000\004\000\000\000\
\\000\000\
\\006\000\003\000\007\000\035\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\006\000\003\000\007\000\037\000\008\000\001\000\000\000\
\\000\000\
\\003\000\039\000\005\000\004\000\000\000\
\\003\000\040\000\005\000\004\000\000\000\
\\003\000\041\000\005\000\004\000\000\000\
\\003\000\042\000\005\000\004\000\000\000\
\\003\000\043\000\005\000\004\000\000\000\
\\003\000\044\000\005\000\004\000\000\000\
\\003\000\045\000\005\000\004\000\000\000\
\\003\000\046\000\005\000\004\000\000\000\
\\003\000\047\000\005\000\004\000\000\000\
\\003\000\048\000\005\000\004\000\000\000\
\\003\000\049\000\005\000\004\000\000\000\
\\003\000\050\000\005\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\058\000\005\000\004\000\009\000\057\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\060\000\005\000\004\000\000\000\
\\003\000\061\000\005\000\004\000\000\000\
\\003\000\062\000\005\000\004\000\000\000\
\\000\000\
\\003\000\063\000\005\000\004\000\000\000\
\\003\000\065\000\004\000\064\000\005\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\058\000\005\000\004\000\009\000\072\000\000\000\
\\003\000\073\000\005\000\004\000\000\000\
\\003\000\074\000\005\000\004\000\000\000\
\\000\000\
\\003\000\065\000\004\000\075\000\005\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\077\000\005\000\004\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 79
val numrules = 40
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
datatype svalue = VOID | ntVOID of unit | ID of  (string) | INT of  (int) | TY of  (Ast.Ty) | EXPCOMM of  (Ast.Expr list) | VARDECL of  (Ast.decl) | DECS of  (Ast.decl list) | DEC of  (Ast.decl) | LVALUE of  (Ast.Lvalue) | EXPS of  (Ast.Expr list) | EXP of  (Ast.Expr) | PROGRAM of  (Ast.Prog) | S of  (Ast.Prog)
end
type svalue = MlyValue.svalue
type result = Ast.Prog
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
of  ( 0, ( ( _, ( MlyValue.PROGRAM PROGRAM, PROGRAM1left, PROGRAM1right)) :: rest671)) => let val  result = MlyValue.S ((*#line 54.19 "expr.grm"*)PROGRAM(*#line 377.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PROGRAM1left, PROGRAM1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 56.17 "expr.grm"*) Ast.PE(EXP) (*#line 381.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.DECS DECS, DECS1left, DECS1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 57.11 "expr.grm"*) Ast.PD(DECS) (*#line 385.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, DECS1left, DECS1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 59.39 "expr.grm"*) Ast.INT(INT) (*#line 389.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, INT1left, INT1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 60.17 "expr.grm"*) Ast.ID(ID) (*#line 393.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 61.39 "expr.grm"*) Ast.BINOP(EXP1, Ast.Plus, EXP2) (*#line 397.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 62.23 "expr.grm"*) Ast.BINOP(EXP1, Ast.Minus, EXP2) (*#line 401.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 63.25 "expr.grm"*) Ast.BINOP(EXP1, Ast.Mul, EXP2) (*#line 405.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 64.24 "expr.grm"*) Ast.BINOP(EXP1, Ast.Div, EXP2) (*#line 409.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 65.26 "expr.grm"*) Ast.BINOP(EXP1, Ast.Equals, EXP2) (*#line 413.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 66.27 "expr.grm"*) Ast.BINOP(EXP1, Ast.NotEqual, EXP2) (*#line 417.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 67.25 "expr.grm"*) Ast.BINOP(EXP1, Ast.LessThan, EXP2) (*#line 421.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 68.29 "expr.grm"*) Ast.BINOP(EXP1, Ast.LessThanEqual, EXP2) (*#line 425.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 69.25 "expr.grm"*) Ast.BINOP(EXP1, Ast.GreThan, EXP2) (*#line 429.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 70.28 "expr.grm"*) Ast.BINOP(EXP1, Ast.GreThanEqual, EXP2) (*#line 433.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 71.31 "expr.grm"*) EXP (*#line 437.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, LVALUE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 72.18 "expr.grm"*) Ast.LVE(LVALUE) (*#line 441.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, LVALUE1left, LVALUE1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 73.33 "expr.grm"*) Ast.ASSIGN(LVALUE, EXP) (*#line 445.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, LVALUE1left, EXP1right), rest671)
end
|  ( 18, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: _ :: ( _, ( MlyValue.DECS DECS, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 74.29 "expr.grm"*) Ast.LET(DECS, EXPS) (*#line 449.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, LET1left, END1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXPCOMM EXPCOMM, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 75.32 "expr.grm"*) Ast.FUNC(ID, EXPCOMM) (*#line 453.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 76.26 "expr.grm"*) Ast.IF(EXP1, EXP2) (*#line 457.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, IF1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 77.32 "expr.grm"*) Ast.IFELSE(EXP1, EXP2, EXP3) (*#line 461.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, IF1left, EXP3right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 78.38 "expr.grm"*) Ast.FOR(ID, EXP1, EXP2, EXP3) (*#line 465.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, FOR1left, EXP3right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 79.24 "expr.grm"*) Ast.WHILE(EXP1, EXP2) (*#line 469.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, WHILE1left, EXP2right), rest671)
end
|  ( 24, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 81.14 "expr.grm"*) Ast.BREAK (*#line 473.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 25, ( rest671)) => let val  result = MlyValue.EXPCOMM ((*#line 83.30 "expr.grm"*) [] (*#line 477.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 26, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.EXPCOMM ((*#line 84.17 "expr.grm"*) EXP::[] (*#line 481.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, EXP1left, EXP1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXPCOMM EXPCOMM, _, EXPCOMM1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPCOMM ((*#line 85.31 "expr.grm"*) EXP::EXPCOMM (*#line 485.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, EXP1left, EXPCOMM1right), rest671)
end
|  ( 28, ( rest671)) => let val  result = MlyValue.EXPS ((*#line 89.27 "expr.grm"*) [] (*#line 489.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 29, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 90.18 "expr.grm"*) EXP::[] (*#line 493.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, EXP1left, EXP1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.EXPS EXPS, _, EXPS1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 91.27 "expr.grm"*) EXP::EXPS (*#line 497.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, EXP1left, EXPS1right), rest671)
end
|  ( 31, ( rest671)) => let val  result = MlyValue.DECS ((*#line 95.28 "expr.grm"*) [] (*#line 501.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 32, ( ( _, ( MlyValue.DEC DEC, DEC1left, DEC1right)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 96.17 "expr.grm"*) DEC::[] (*#line 505.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, DEC1left, DEC1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.DECS DECS, _, DECS1right)) :: _ :: ( _, ( MlyValue.DEC DEC, DEC1left, _)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 97.27 "expr.grm"*) DEC::DECS (*#line 509.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, DEC1left, DECS1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.VARDECL VARDECL, VARDECL1left, VARDECL1right)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 100.24 "expr.grm"*) VARDECL (*#line 513.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, VARDECL1left, VARDECL1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.TY ((*#line 102.16 "expr.grm"*)Ast.TYPEID(ID)(*#line 517.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, ID1left, ID1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.VARDECL ((*#line 104.34 "expr.grm"*) Ast.VARDECL(ID, EXP) (*#line 521.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, VAR1left, EXP1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 106.22 "expr.grm"*)Ast.LVAL_IDEN(ID)(*#line 525.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 107.22 "expr.grm"*)Ast.FIELD (LVALUE,ID)(*#line 529.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, LVALUE1left, ID1right), rest671)
end
|  ( 39, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 108.31 "expr.grm"*)Ast.ELEMENT (LVALUE,EXP)(*#line 533.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, LVALUE1left, RPAREN1right), rest671)
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
