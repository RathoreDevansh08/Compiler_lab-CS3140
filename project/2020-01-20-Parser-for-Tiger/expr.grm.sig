signature Tiger_TOKENS =
sig
type ('a,'b) token
type svalue
val IMPORT:  'a * 'a -> (svalue,'a) token
val BACKSLASH:  'a * 'a -> (svalue,'a) token
val DOUBQUOTES:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val STRINGK:  'a * 'a -> (svalue,'a) token
val INTK:  'a * 'a -> (svalue,'a) token
val NIL:  'a * 'a -> (svalue,'a) token
val OF:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val ARRAY:  'a * 'a -> (svalue,'a) token
val TYPE:  'a * 'a -> (svalue,'a) token
val NEW:  'a * 'a -> (svalue,'a) token
val BREAK:  'a * 'a -> (svalue,'a) token
val TO:  'a * 'a -> (svalue,'a) token
val FOR:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val RCUBR:  'a * 'a -> (svalue,'a) token
val LCUBR:  'a * 'a -> (svalue,'a) token
val RSQBR:  'a * 'a -> (svalue,'a) token
val LSQBR:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val FUNCTION:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val IN:  'a * 'a -> (svalue,'a) token
val LET:  'a * 'a -> (svalue,'a) token
val GRETHANEQUAL:  'a * 'a -> (svalue,'a) token
val GRETHAN:  'a * 'a -> (svalue,'a) token
val LESSTHANEQUAL:  'a * 'a -> (svalue,'a) token
val LESSTHAN:  'a * 'a -> (svalue,'a) token
val NOTEQUAL:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val EQUALS:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val DIVIDE:  'a * 'a -> (svalue,'a) token
val MULTIPLY:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Tiger_LRVALS=
sig
structure Tokens : Tiger_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
