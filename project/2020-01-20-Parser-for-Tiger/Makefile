.PHONY: all clean

all: tigh

%.lex.sml: %.lex
		mllex $<
		
%.grm.sml: %.grm
		mlyacc $<

tigh: tiger.mlb ast.sml prettyprint.sml expr.grm.sig expr.grm.sml tiger.lex.sml tigh.sml
		mlton -output tigh  tiger.mlb

clean:
		rm tigh
