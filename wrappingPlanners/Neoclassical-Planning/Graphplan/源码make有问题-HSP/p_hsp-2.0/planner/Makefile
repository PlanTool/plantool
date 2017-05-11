CC	= gcc
YACC	= yacc
FLEX	= flex
CFLAGS	= -Wall -O3 -DCOMPETITION_OUTPUT
SED	= sed


compile:
			@echo "*** Compiling all in `pwd`"
			@make hsp

hsp:			planner.o parser.o pddl.o
			$(CC) -o hsp planner.o parser.o pddl.o

planner.o:		planner.c parser.h planner.h
			$(CC) $(CFLAGS) -c planner.c

parser.c:		parser.y
			$(YACC) -v -d -t parser.y
			@mv y.tab.c parser.c
			@mv y.tab.h tokens.h

parser.o:		parser.c parser.h tokens.h planner.h
			$(CC) $(CFLAGS) -c parser.c

pddl.c:			pddl.lex
			$(FLEX) -i -t pddl.lex > pddl.c

pddl.o:			pddl.c parser.h tokens.h
			$(CC) $(CFLAGS) -c pddl.c

clean:
			@echo "*** Cleaning all in `pwd`"
			@rm -f *.o pddl.c parser.c hsp y.output
