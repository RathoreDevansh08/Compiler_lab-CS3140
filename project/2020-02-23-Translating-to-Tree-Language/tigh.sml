structure EC =
struct

structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
structure TigerLex    = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerParser = Join( structure ParserData = TigerLrVals.ParserData
			     structure Lex        = TigerLex
			     structure LrParser   = LrParser
			   )

(* 
	At this point every thing regarding lexing and parsing is contained in
	the TigerParser structure. Let us create a lexer using this.
*)
(*  Build Lexers  *)
fun makeTigerLexer strm = TigerParser.makeLexer (fn n => TextIO.inputN(strm,n))
val makeFileLexer      = makeTigerLexer o TextIO.openIn


(*  Parse command line and set a suitable lexer  *)
val thisLexer = makeTigerLexer TextIO.stdIn

fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr,
					    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")


val (program,_) = TigerParser.parse (0,thisLexer,print_error,()) (* parsing *)
val executable  = PrettyPrint.compile program

end
