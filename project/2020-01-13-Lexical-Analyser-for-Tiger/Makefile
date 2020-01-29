.PHONY: all clean

all: tigh

%.lex.sml: %.lex
		mllex $<

tigh: tiger.mlb tokens.sml term.sml  tiger.lex.sml tigh.sml
		mlton -output tigh  tiger.mlb

clean:
		rm tigh
