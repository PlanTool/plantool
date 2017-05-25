#!/bin/sh
#
# Makefile for IPP v4
#


####### FLAGS

CC      = gcc

CFLAGS	= -O6 -Wall -ansi -fPIC

LIBS    = -lm

INC_PYTHON = -I/usr/include/python2.7

####### Files

PDDL_PARSER_SRC	= scan-fct_pddl.tab.c \
	scan-ops_pddl.tab.c \
	scan-probname.tab.c \
	lex.fct_pddl.c \
	lex.ops_pddl.c 

PDDL_PARSER_OBJ = scan-fct_pddl.tab.o \
	scan-ops_pddl.tab.o 

SOURCES 	= main.c \
	output.c \
	utilities.c \
	memory.c \
	pddl.c \
	pddl-types.c \
	instantiateI.c \
	instantiateII.c \
	instantiateIII.c \
	instantiateIV.c \
	build_graph.c \
	exclusions.c \
	search_plan.c \
	memoize.c \
	wave_front.c \
	save_graph.c


OBJECTS 	= $(SOURCES:.c=.o)

####### Implicit rules

.SUFFIXES:

.SUFFIXES: .c .o

.c.o:; $(CC) -c $(CFLAGS) $<

####### Build rules

_ipp.so: $(OBJECTS) $(PDDL_PARSER_OBJ) ipp_wrap.o
	gcc -shared -o $@ $^

ipp_wrap.o: ipp_wrap.c
	$(CC) -c $(INC_PYTHON) $(CFLAGS) $^

ipp.py ipp_wrap.c: ipp.i
	swig -python ipp.i

ipp: $(OBJECTS) $(PDDL_PARSER_OBJ)
	$(CC) -o ipp $(OBJECTS) $(PDDL_PARSER_OBJ) $(CFLAGS) $(LIBS)

# v4 with pddl syntax
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
	rm -f *.o *.bak *~ *% core *_pure_p9_c0_400.o.warnings \
        \#*\# $(PDDL_PARSER_SRC)

veryclean: clean
	rm -f ipp graph.* *.symbex core\
	$(PDDL_PARSER_SRC) \
	lex.fct_pddl.c lex.ops_pddl.c lex.probname.c \
	lex.fct_res.c lex.ops_res.c \
	*.output

depend:
	makedepend -- $(SOURCES) $(RES_PARSER_SRC) $(PDDL_PARSER_SRC)

lint:
	lclint -booltype Bool $(SOURCES) 2> output.lint

# DO NOT DELETE
