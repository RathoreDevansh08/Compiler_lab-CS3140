(* user declarations - ML-Lex definitions - rules *)
type lexresult     = Token
fun eof ()  	   = EOF

fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt        = toSigned o String.explode
val newlineCount = List.length o List.filter (fn x => x = #"\n") o String.explode

%%

%structure Tiger_Lex
ws    = [\ \t];
digit = [0-9]+;
letter = [a-zA-Z];
esc = [\a \b \f \r \t \v];

%%

"\""([^"])*"\""						=> ( COMMENT yytext );
"/*"(\n | \*[^/] | [^*])*"*/"       => ( COMMENT yytext );
{esc}	  => ( print(yytext); lex() );
{ws}+         => ( print(yytext); lex() );
\n({ws}*\n)*  => ( print(yytext); lex() );
{digit}+      => ( NUMBER ( toInt yytext) );
while	  	  => ( KEYWORD yytext);
for			  => ( KEYWORD yytext);
to			  => ( KEYWORD yytext);
break		  => ( KEYWORD yytext);
let			  => ( KEYWORD yytext);
in			  => ( KEYWORD yytext);
end			  => ( KEYWORD yytext);
function	  => ( KEYWORD yytext);
var			  => ( KEYWORD yytext);
type		  => ( KEYWORD yytext);
array		  => ( KEYWORD yytext);
if			  => ( KEYWORD yytext);
then		  => ( KEYWORD yytext);
else		  => ( KEYWORD yytext);
do			  => ( KEYWORD yytext);
of			  => ( KEYWORD yytext);
nil			  => ( KEYWORD yytext);
int     	  => ( KEYWORD yytext);   
string  	  => ( KEYWORD yytext);
"+"           => ( SYMBOL yytext );
"-"           => ( SYMBOL yytext );
"*"           => ( SYMBOL yytext );
"/"           => ( SYMBOL yytext );
","			  => ( SYMBOL yytext );
":"			  => ( SYMBOL yytext );
";"			  => ( SYMBOL yytext );
"."			  => ( SYMBOL yytext );
"="			  => ( SYMBOL yytext );
"<>"		  => ( SYMBOL yytext );
"<"			  => ( SYMBOL yytext );
"<="		  => ( SYMBOL yytext );
">"			  => ( SYMBOL yytext );
">="		  => ( SYMBOL yytext );
"&"			  => ( SYMBOL yytext );
"|"			  => ( SYMBOL yytext );
":="		  => ( SYMBOL yytext );
"("			  => ( SYMBOL yytext );
")"			  => ( SYMBOL yytext );
"["			  => ( SYMBOL yytext );
"]"			  => ( SYMBOL yytext );
"{"			  => ( SYMBOL yytext );
")"			  => ( SYMBOL yytext );
"\""		  => ( SYMBOL yytext );
"\'"		  => ( SYMBOL yytext );
"\\"		  => ( SYMBOL yytext );
[a-zA-Z][a-zA-Z0-9_]* => ( IDENTIFIER yytext);
