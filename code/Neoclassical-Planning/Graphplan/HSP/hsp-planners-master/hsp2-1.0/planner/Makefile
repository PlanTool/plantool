CC	= gcc
C++	= g++
LD	= $(CC)
YACC	= bison
FLEX	= flex
ARCH	= #-m64
CFLAGS	= $(ARCH) -Wall -O3 -DCOMPETITION_OUTPUT
#CFLAGS	= -Wall -g -DCOMPETITION_OUTPUT
LDFLAGS	= $(ARCH)


compile:
			@echo "*** Compiling all in `pwd`"
			@make hsp2

hsp2:			planner.o parser.o pddl.o
			$(LD) $(LDFLAGS) -o hsp2 planner.o parser.o pddl.o

planner.o:		planner.c parser.h planner.h
			$(CC) $(CFLAGS) -c planner.c

parser.c:		parser.y
			$(YACC) -v -d -t parser.y
			mv parser.tab.c parser.c
			mv parser.tab.h tokens.h
			rm parser.output

parser.o:		parser.c parser.h tokens.h planner.h
			$(CC) $(CFLAGS) -c parser.c

pddl.c:			pddl.lex
			$(FLEX) -i -t pddl.lex > pddl.c

pddl.o:			pddl.c parser.h tokens.h
			$(CC) $(CFLAGS) -c pddl.c

clean:
			@echo "*** Cleaning all in `pwd`"
			@rm -f *.o pddl.c parser.c hsp2 y.output *~

