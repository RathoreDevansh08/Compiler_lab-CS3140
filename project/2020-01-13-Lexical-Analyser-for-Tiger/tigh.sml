val interactive = Tiger_Lex.makeLexer (fn _ => TextIO.inputN (TextIO.stdIn,1))

fun lexfile file = let val strm = TextIO.openIn file
		   		   in Tiger_Lex.makeLexer (fn n => TextIO.inputN(strm,n))
		    	   end

fun runWithLexer lexer = let fun loop () = case lexer () of
						       EOF       => print("")
					        |  x         => loop (Term.highlight x)
			 			 in    loop ()
			 			 end


val _ =  ( case CommandLine.arguments() of
	       		[] => runWithLexer interactive
	    	  | xs => (List.map (runWithLexer o lexfile) xs; ())
	     )
