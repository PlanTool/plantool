YACC = yacc
LEX = flex --case-insensitive
SED = sed
CC = gcc
CFLAGS = -g -Wall



compile:
		@echo "*** Compiling all in `pwd`"
		@make parser


parser:		parser.o pddl.o
		@$(CC) -o parser parser.o pddl.o

parser.c:	parser.y
		@$(YACC) -v -d -t parser.y
		@mv y.tab.c parser.c
		@mv y.tab.h parser.h

pddl.c:		pddl.lex
		@$(LEX) -t pddl.lex | $(SED) "s/getc(yyin)/toupper(getc(yyin))/" > pddl.c

parser.o:	parser.c parser.h
		@$(CC) -Wall -g -c parser.c

pddl.o:		pddl.c parser.h
		@$(CC) -Wall -g -c pddl.c


clean:
		@echo "*** Cleaning all in `pwd`"
		@rm -f parser parser.c parser.h pddl.c *.o y.output


.SILENT:

