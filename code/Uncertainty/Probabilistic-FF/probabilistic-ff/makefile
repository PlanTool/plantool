#!/bin/sh
#
# Makefile for FF v 1.0
#


####### FLAGS

TYPE	= 
ADDONS	= 

CC      = gcc
CPP      = g++

CFLAGS	= -O6 -Wall -g -ansi
# -g -pg
CPPFLAGS = -O6 -Wall -g -ansi -Wno-deprecated


LIBS    = -lm -L Cachet-1.21-wmc/ -L MiniSat_v1.14/ -lsat


####### Files

PDDL_PARSER_SRC	= scan-fct_pddl.tab.c \
	scan-ops_pddl.tab.c \
	scan-probname.tab.c \
	lex.fct_pddl.c \
	lex.ops_pddl.c 

PDDL_PARSER_OBJ = scan-fct_pddl.tab.o \
	scan-ops_pddl.tab.o 


CSOURCES 	= main.c \
	memory.c \
	output.c \
	parse.c \
	inst_pre.c \
	inst_easy.c \
	inst_hard.c \
	inst_final.c \
	search.c \
	repeated_states.c

CPPSOURCES 	= state_transitions.cpp \
	relax.cpp \
	Solver.cpp


COBJECTS 	= $(CSOURCES:.c=.o)

CPPOBJECTS 	= $(CPPSOURCES:.cpp=.o)




####### Implicit rules

.SUFFIXES:

.SUFFIXES: .c .o

.c.o:; $(CC) -c $(CFLAGS) $<

.cpp.o: $(CPP) -c $(CPPFLAGS) $<



####### Build rules


ff: $(COBJECTS) $(CPPOBJECTS) $(PDDL_PARSER_OBJ)
	$(CPP) -o ff $(COBJECTS) $(CPPOBJECTS) $(PDDL_PARSER_OBJ) $(CPPFLAGS) $(LIBS)

state_transitions.o: state_transitions.cpp
	$(CPP) -c $(CPPFLAGS) state_transitions.cpp

relax.o: relax.cpp
	$(CPP) -c $(CPPFLAGS) relax.cpp

Solver.o: Solver.cpp
	$(CPP) -c $(CPPFLAGS) Solver.cpp


# pddl syntax
scan-fct_pddl.tab.c: scan-fct_pddl.y lex.fct_pddl.c
	bison -pfct_pddl -bscan-fct_pddl scan-fct_pddl.y

scan-ops_pddl.tab.c: scan-ops_pddl.y lex.ops_pddl.c
	bison -pops_pddl -bscan-ops_pddl scan-ops_pddl.y

lex.fct_pddl.c: lex-fct_pddl.l
	flex -Pfct_pddl lex-fct_pddl.l

lex.ops_pddl.c: lex-ops_pddl.l
	flex -Pops_pddl lex-ops_pddl.l


# misc
clean:
	rm -f A CNF *.o *.bak *~ *% core *_pure_p9_c0_400.o.warnings \
        \#*\# $(RES_PARSER_SRC) $(PDDL_PARSER_SRC) semantic.cache

veryclean: clean
	rm -f ff L* graph.* *.symbex gmon.out \
	$(PDDL_PARSER_SRC) \
	lex.fct_pddl.c lex.ops_pddl.c lex.probname.c \
	*.output

depend:
	makedepend -- $(SOURCES) $(PDDL_PARSER_SRC)

lint:
	lclint -booltype Bool $(SOURCES) 2> output.lint

# DO NOT DELETE
