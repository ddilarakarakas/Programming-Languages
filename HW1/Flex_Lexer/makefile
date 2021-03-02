GCC = gcc
CFLAGS = -pedantic -Werror
LIBS = -lm

gpp_lexer.out: gpp_lexer.c
	$(GCC) gpp_lexer.c -o gpp_lexer.out $(CFLAGS) $(LIBS)

gpp_lexer.c: gpp_lexer.l
	flex -o gpp_lexer.c gpp_lexer.l

clean:
	rm -f gpp_lexer.out
	rm -f gpp_lexer.c
