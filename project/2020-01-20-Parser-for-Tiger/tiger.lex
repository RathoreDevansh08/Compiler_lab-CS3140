(* user declarations - ML-Lex definitions - rules *)
type pos = int

type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
(* type lexresult = Tiger.token *)
type lexresult     = (svalue,pos) token

fun eof ()  	   = Tokens.EOF(0,0)

fun IntFromString str = let
                            val x = Int.fromString str
                        in
                            case x of
                                    SOME n => n
                                |   NONE   => 0
                        end

val nested_commenting = ref 0;


%%

%header (functor TigerLexFun (structure Tokens : Tiger_TOKENS));
%s COMMENT;
ws    = [\ \t];
digit = [0-9]+;
letter = [a-zA-Z];
eol = ("\n\r" | "\r\n" | "\r" | "\n");
esc = [\a \b \f \r \t \v];

%%

<INITIAL> "/*"    => ( nested_commenting := !nested_commenting + 1;
					   YYBEGIN COMMENT; continue() );

<COMMENT> "/*"    => ( nested_commenting := !nested_commenting + 1;
                       continue() );

<COMMENT> "*/"    => ( nested_commenting := !nested_commenting - 1;
                       if (!nested_commenting = 0) then (YYBEGIN INITIAL; continue())
                       else continue() );
                
<COMMENT> . | [\n]       => ( continue() );

<INITIAL> {eol} =>            ( continue() );
<INITIAL> {esc}	  => ( continue() );
<INITIAL> {ws}+         => ( continue() );

<INITIAL> {digit}+      => ( Tokens.INT(IntFromString yytext, yypos, yypos + size yytext) );


<INITIAL> "let"       => ( Tokens.LET(yypos, yypos + 3) );
<INITIAL> "in"        => ( Tokens.IN(yypos, yypos + 2)) ;
<INITIAL> "end"       => ( Tokens.END(yypos, yypos + 3) );
<INITIAL> "var"       => ( Tokens.VAR(yypos, yypos + 3) );
<INITIAL> "if"        => ( Tokens.IF(yypos, yypos + 2) );
<INITIAL> "then"      => ( Tokens.THEN(yypos, yypos + 4) );
<INITIAL> "else"      => ( Tokens.ELSE(yypos, yypos + 4) );
<INITIAL> "function"  => ( Tokens.FUNCTION(yypos, yypos + 8) );
<INITIAL> "while"	  => ( Tokens.WHILE(yypos, yypos + 5) );
<INITIAL> "for"		  => ( Tokens.FOR(yypos, yypos + 3) );
<INITIAL> "to"		  => ( Tokens.TO(yypos, yypos + 2) );
<INITIAL> "break"	  => ( Tokens.BREAK(yypos, yypos + 5) );
<INITIAL> "type"	  => ( Tokens.TYPE(yypos, yypos + 4) );
<INITIAL> "array"	  => ( Tokens.ARRAY(yypos, yypos + 5) );
<INITIAL> "do"		  => ( Tokens.DO(yypos, yypos + 2) );
<INITIAL> "of"		  => ( Tokens.OF(yypos, yypos + 2) );
<INITIAL> "nil"		  => ( Tokens.NIL(yypos, yypos + 3) );
<INITIAL> "int"       => ( Tokens.INTK(yypos, yypos + 3) );
<INITIAL> "string"    => ( Tokens.STRINGK(yypos, yypos + 6) );

<INITIAL> "+"         => ( Tokens.PLUS (yypos, yypos+1) );
<INITIAL> "-"         => ( Tokens.MINUS (yypos, yypos+1) );
<INITIAL> "*"         => ( Tokens.MULTIPLY (yypos, yypos+1) );
<INITIAL> "/"         => ( Tokens.DIVIDE (yypos, yypos+1) );
<INITIAL> ","		  => ( Tokens.COMMA(yypos, yypos + 1) );
<INITIAL> ":"		  => ( Tokens.COLON(yypos, yypos + 1) );
<INITIAL> ";"		  => ( Tokens.SEMICOLON (yypos, yypos+1) );
<INITIAL> "."		  => ( Tokens.DOT (yypos, yypos+1) );
<INITIAL> "="		  => ( Tokens.EQUALS(yypos, yypos + 1) );
<INITIAL> "<>"		  => ( Tokens.NOTEQUAL(yypos, yypos + 2) );
<INITIAL> "<"		  => ( Tokens.LESSTHAN(yypos, yypos + 1) );
<INITIAL> "<="		  => ( Tokens.LESSTHANEQUAL(yypos, yypos + 2) );
<INITIAL> ">"		  => ( Tokens.GRETHAN(yypos, yypos + 1) );
<INITIAL> ">="		  => ( Tokens.GRETHANEQUAL(yypos, yypos + 2) );
<INITIAL> "&"		  => ( Tokens.AND(yypos, yypos + 1) );
<INITIAL> "|"		  => ( Tokens.OR(yypos, yypos + 1) );
<INITIAL> ":="		  => ( Tokens.ASSIGN (yypos, yypos + 2) );
<INITIAL> "("         => ( Tokens.LPAREN(yypos, yypos + 1) );
<INITIAL> ")"		  => ( Tokens.RPAREN(yypos, yypos + 1) );
<INITIAL> "["		  => ( Tokens.LSQBR(yypos, yypos + 1) );
<INITIAL> "]"		  => ( Tokens.RSQBR(yypos, yypos + 1) );
<INITIAL> "{"		  => ( Tokens.LCUBR(yypos, yypos + 1) );
<INITIAL> "}"		  => ( Tokens.RCUBR(yypos, yypos + 1) );

<INITIAL> [a-zA-Z][a-zA-Z0-9_]* => ( Tokens.ID(yytext, yypos, yypos + size yytext) );

<INITIAL> "\""		  => ( Tokens.DOUBQUOTES(yypos, yypos + 1) );
<INITIAL> "\\"		  => ( Tokens.BACKSLASH(yypos, yypos + 1) );
