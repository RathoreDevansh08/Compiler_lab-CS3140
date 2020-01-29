# Lexical analyser for Tiger (Project component).

**NOTE:**This weeks assignment has only the project component and
hence all code that you write as part of this assignment should go to
your project subdirectory of your repository.

The aim of this weeks assignment is to

1. Understand the lexical analysis phase of a compiler

2. Use the [`ml-lex`][ml-lex] to write the lexical analyser phase for your tiger
   compiler. The lexical analyser that you have will need some
   tweaking later on (particularly to make it work with your `ml-yacc`
   parser generator).

3. We are going to write a program `tigh` which will highlight a tiger
   program, i.e. will add colours when printed to an ascii
   terminal. For this you will need only a lexer (and not a parser).


## Overall structure.

1. Refer the end of [Appel's book][modern] for the syntax of tiger and
   identify the tokens of the tiger language. An alternate online
   source is https://www.lrde.epita.fr/~tiger/tiger.html.

2. Add a `tokens.sml` file that defines the data type `Token` that captures
   tokens of the tiger language.
   ```
   $ cat tokens.sml

   datatype Token = ...

   ```

3. Add the files `tiger.lex` for input to `mllex` and `tigh.sml` for the
   driver syntax highlighter.

4. For highlighting you can use ANSI codes for setting the properties
   of text on your terminal. Write a separate file `term.sml` that
   contains helper functions for setting the properties of text on the
   terminal.

5. You will also need to update your `Makefile` to have a target
   called `tigh` which builds the highlighter.


## Note about ASCII codes.

On an ascii terminal (most terminals support ascii colour) you change
the text properties by sending a special sequence of characters. This
special sequence start with the character `ESC` (ascii 27) followed by
the character `[` followed by a sequence of command characters as
documented in the section [Select Graphic Rendition][sgr] of Wikipedia
entry for [ANSI escape code][ansi-codes]. For example, to set the
forground to red you need to output the string sequence `ESC[31m`
where ESC is the escape character (ascii 27).  You can use the `chr :
int -> char` to convert from ascii code to the corresponding
character.

## Handling spaces.

If your lexer skips whitespaces and new lines like what all good
lexers do, you will have problem with preserving spaces. To avoid this
problem follow the steps below.

1. Keep track of the position in the stream in terms of line and
   columns. The code below can be added to the lexer file at
   appropriate place.
      ```
   type Pos = int * int (* line, column pair *)
   val curpos = ref (0,0)
   fun newlines n = ... (* code to advance the position by n lines *)
   fun spaces n   = ... (* code to advance the column by n spaces *)
   fun annotate (t : Token) = ...
		(* annotate  token with the current position *)
   ```

2. Instead of returning just the token return a tuple of `Token *
   Position`. Use the helper functions defined in the code segment
   above to make your lexer action efficient.

3. Your driver program should insert in the output stream enough
   spaces and new lines.


[modern]: <https://www.cs.princeton.edu/~appel/modern/ml/>
[sgr]: <https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters>
[ansi-codes]: <https://en.wikipedia.org/wiki/ANSI_escape_code>
[ml-lex]: <http://www.smlnj.org/doc/ML-Lex/manual.html>
