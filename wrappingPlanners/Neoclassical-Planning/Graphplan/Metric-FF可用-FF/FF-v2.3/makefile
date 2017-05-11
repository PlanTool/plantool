#!/bin/sh
#
# Makefile for FF v 1.0
#


####### FLAGS

TYPE	= -fPIC
ADDONS	= 

CC      = gcc

INC_PYTHON = -I/usr/include/python2.7

# changed from -ansi to -std=gnu99.  -ansi has different meanings on Mac and Linux.
# on Linux, it doesn't allow use of random() and srandom() (see inst_final.c).
# [2016/09/28:rpg]
CFLAGS	= -Wall -g -std=gnu99 $(TYPE) $(ADDONS) 
# -g -pg

ifeq ($(OS),Windows_NT)
	$(error FF unsupported on Windows)
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
        CFLAGS += -O6
    endif
    ifeq ($(UNAME_S),Darwin)
        CFLAGS += -O3 # -O6 not supported by Apple's compiler
    endif
endif

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
	orderings.c \
	relax.c \
	search.c

OBJECTS 	= $(SOURCES:.c=.o)

####### Implicit rules

.SUFFIXES:

.SUFFIXES: .c .o

.c.o:; $(CC) -c $(CFLAGS) $<

####### Build rules

_ff.so: $(OBJECTS) ff_wrap.o $(PDDL_PARSER_OBJ)
	gcc $(CFLAGS) -shared -o $@ $^

ff_wrap.o: ff_wrap.c
	$(CC) -c $(CFLAGS) $(INC_PYTHON) $^

ff.py ff_wrap.c: ff.i
		swig -python ff.i

ff: $(OBJECTS) $(PDDL_PARSER_OBJ)
	$(CC) -o ff $(OBJECTS) $(PDDL_PARSER_OBJ) $(CFLAGS) $(LIBS)

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
	rm -f *.o *.bak *~ *% core *_pure_p9_c0_400.o.warnings \
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
