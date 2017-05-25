#!/bin/sh
#
# Makefile for FF v 1.0
#


####### FLAGS

TYPE	= -fPIC
ADDONS	= 

CC      = gcc

CFLAGS	= -O6 -Wall -g -ansi -fPIC
# -g -pg
INC_PYTHON = -I/usr/include/python2.7

LIBS    = -lm


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
	inst_pre.c \
	inst_easy.c \
	inst_hard.c \
	inst_final.c \
	relax.c \
	search.c \
	state_transitions.c \
	repeated_states.c


OBJECTS 	= $(SOURCES:.c=.o)




####### Implicit rules

.SUFFIXES:

.SUFFIXES: .c .o

.c.o:; $(CC) -c $(CFLAGS) $<



####### Build rules
_Conf_FF.so: $(OBJECTS) $(PDDL_PARSER_OBJ) Conf_FF_wrap.o
	gcc -shared -o $@ $^ 

Conf_FF_wrap.o: Conf_FF_wrap.c
	$(CC) -c $(INC_PYTHON) -fPIC $^

Conf_FF.py Conf_FF_wrap.c: Conf_FF.i
	swig -python Conf_FF.i

Conf_FF: $(OBJECTS) $(PDDL_PARSER_OBJ)
	$(CC) -o Conf_FF $(OBJECTS) $(PDDL_PARSER_OBJ) $(CFLAGS) $(LIBS)




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
        \#*\# $(RES_PARSER_SRC) $(PDDL_PARSER_SRC)

veryclean: clean
	rm -f ff H* J* K* L* O* graph.* *.symbex gmon.out \
	$(PDDL_PARSER_SRC) \
	lex.fct_pddl.c lex.ops_pddl.c lex.probname.c \
	*.output

depend:
	makedepend -- $(SOURCES) $(PDDL_PARSER_SRC)

lint:
	lclint -booltype Bool $(SOURCES) 2> output.lint

# DO NOT DELETE
