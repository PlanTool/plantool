#!/bin/sh
#
# Makefile for ff(h_a) 
# contains minimal changes to Metric-FFs makefile


####### FLAGS

TYPE	= -fPIC
ADDONS	= 

CC      = gcc
# CC      = g++

CFLAGS	= -g -O3 -Wall -ansi $(TYPE) $(ADDONS) 
# -pg -O6 -fmudflap

INC_PYTHON = -I/usr/include/python2.7

LIBS    = -lm
# -lmudflap


####### Files

PDDL_PARSER_SRC	= scan-fct_pddl.tab.c \
	scan-ops_pddl.tab.c \
	scan-probname.tab.c \
	lex.fct_pddl.c \
	lex.ops_pddl.c 

PDDL_PARSER_OBJ = scan-fct_pddl.tab.o \
	scan-ops_pddl.tab.o 


SOURCES 	= main.c \
	memory.c \
	output.c \
	parse.c \
	expressions.c \
	inst_pre.c \
	inst_easy.c \
	inst_hard.c \
	inst_final.c \
	search.c \
	list_set.c \
	graph.c \
	heuristics.c \
	preprocess.c \
	distances.c \
	utility.c


OBJECTS 	= $(SOURCES:.c=.o)

####### Implicit rules

.SUFFIXES:

.SUFFIXES: .c .o

.c.o:; $(CC) -c $(CFLAGS) $<

####### Build rules

#

Seq_Sat_FF: $(OBJECTS) $(PDDL_PARSER_OBJ)
	$(CC) -o Seq_Sat_FF $(OBJECTS) $(PDDL_PARSER_OBJ) $(CFLAGS) $(LIBS)

_Seq_Sat_FF.so: $(OBJECTS) $(PDDL_PARSER_OBJ) Seq_Sat_FF_wrap.o
	gcc -shared -o $@ $^

Seq_Sat_FF_wrap.o: Seq_Sat_FF_wrap.c
	$(CC) -c $(INC_PYTHON) -fPIC $^

Seq_Sat_FF.py Seq_Sat_FF_wrap.c: Seq_Sat_FF.i
	swig -python Seq_Sat_FF.i

Seq_Sat_FF: $(OBJECTS) $(PDDL_PARSER_OBJ)
	$(CC) -o Seq_Sat_FF $(OBJECTS) $(PDDL_PARSER_OBJ) $(CFLAGS) $(LIBS)

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

semiclean:
	rm -f *.o *.bak *~ *% core *_pure_p9_c0_400.o.warnings

clean:
	rm -f *.o *.bak *~ *% core *_pure_p9_c0_400.o.warnings \
        \#*\# $(RES_PARSER_SRC) $(PDDL_PARSER_SRC)

veryclean: clean
	rm -f ffha \
	$(PDDL_PARSER_SRC) \
	lex.fct_pddl.c lex.ops_pddl.c lex.probname.c \
	*.output



depend:
	makedepend -- $(SOURCES) $(PDDL_PARSER_SRC)

lint:
	lclint -booltype Bool $(SOURCES) 2> output.lint

# DO NOT DELETE
