#!/bin/sh
#
# Makefile for FF v 1.0
#


####### FLAGS

TYPE	= 
ADDONS	= 

CC      = gcc

SRC_HEADER =../include

PATH_HEADERS = -I$(SRC_HEADER)

LIBS    = -lm --stats


####### Files

PDDL_PARSER_SRC	= scan-fct_pddl.tab.c \
	scan-ops_pddl.tab.c \
	scan-probname.tab.c \
	lex.dis_fct_pddl.c \
	lex.dis_ops_pddl.c 

PDDL_PARSER_OBJ = scan-fct_pddl.tab.o \
	scan-ops_pddl.tab.o 


SOURCES 	= dis_main.c \
	dis_invariant.c\
	dis_transitive_graph.c\
	dis_3constraints.c\
	dis_6constraints.c\
	dis_constraints.c\
	dis_producible.c\
	dis_memory.c \
	dis_output.c \
	dis_parse.c \
	dis_expressions.c \
	dis_inst_pre.c \
	dis_inst_easy.c \
	dis_inst_hard.c \
	dis_inst_final.c \
	dis_relax.c \
	dis_search.c\
	dis_add_dp.c

OBJECTS 	= $(SOURCES:.c=.o)

find_header = $(wildcard $(dir)/*.h)

HEADERS := $(foreach dir,../include,$(find_header))
 
####### Implicit rules

.SUFFIXES:

.SUFFIXES: .c .o

.c.o:; $(CC) $(PATH_HEADERS) -c $(CFLAGS) $<

####### Build rules

mff.a: $(OBJECTS) $(PDDL_PARSER_OBJ)
	$(AR) rc $@ $^
    
ff: $(OBJECTS) $(PDDL_PARSER_OBJ)
	$(CC) -o ff $(OBJECTS) $(PDDL_PARSER_OBJ) $(CFLAGS) $(LIBS)

$(OBJECTS): $(HEADERS)

# pddl syntax
scan-fct_pddl.tab.c: scan-fct_pddl.y lex.dis_fct_pddl.c
	bison --verbose -pdis_fct_pddl -bscan-fct_pddl scan-fct_pddl.y

scan-ops_pddl.tab.c: scan-ops_pddl.y lex.dis_ops_pddl.c
	bison --verbose -pdis_ops_pddl -bscan-ops_pddl scan-ops_pddl.y
lex.dis_fct_pddl.c: lex-fct_pddl.l
	flex -Pdis_fct_pddl lex-fct_pddl.l

lex.dis_ops_pddl.c: lex-ops_pddl.l
	flex -Pdis_ops_pddl lex-ops_pddl.l


# misc
clean:
	rm -f *.o *.bak *~ *% core mff.a *_pure_p9_c0_400.o.warnings \
        \#*\# $(RES_PARSER_SRC) 
	rm -f ff H* J* K* L* O* graph.* *.symbex gmon.out \
	$(PDDL_PARSER_SRC) \
	lex.dis_fct_pddl.c lex.dis_ops_pddl.c lex.probname.c \
	*.output

depend:
	makedepend -- $(SOURCES) $(PDDL_PARSER_SRC)

lint:
	lclint -booltype dis_Bool $(SOURCES) 2> output.lint

# DO dis_NOT DELETE



