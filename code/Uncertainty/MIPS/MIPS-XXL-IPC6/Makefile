#!/bin/sh
#
# Makefile for MIPS-XXL v 2.0. Based on the Makefile for FF v1.0.
#


####### FLAGS

TYPE	= 
ADDONS	= ## -DFREEMEM -- Disabled for competition version.  
#-DVERIFY_FLUSH 

CC      = gcc

CFLAGS	= -Wall -ansi $(TYPE) $(ADDONS) -O3 -funroll-loops #-DTESTING_EX #  -g -pg
LIBS    = -lm

MIPSFLAGS = -DTESTING_EX #-DFASTER_HEURISTIC_IN_EXTERNAL

####### Files

PDDL_PARSER_SRC	= scan-fct_pddl.tab.c \
	scan-ops_pddl.tab.c \
	scan-xml.tab.c\
	scan-probname.tab.c \
	lex.fct_pddl.c \
	lex.ops_pddl.c \
	lex.xml.c

PDDL_PARSER_OBJ = scan-fct_pddl.tab.o \
	scan-ops_pddl.tab.o \
	scan-xml.tab.o


SOURCES 	= main.c \
	memory.c \
	output.c \
	parse.c \
	grounded.c \
	expressions.c \
	inst_pre.c \
	inst_easy.c \
	inst_hard.c \
	inst_final.c \
	relax.c \
	file.c \
	bucket.c \
	plan_reconstruction.c \
	duplicates_removal.c \
	external_prio_queue.c \
	external.c \
	search.c 

EXTERNAL_SOURCES = 	file.c \
	bucket.c \
	plan_reconstruction.c \
	duplicates_removal.c \
	external_prio_queue.c \
	external.c 


OBJECTS 	= $(SOURCES:.c=.o)
HEADERS         = $(SOURCES:.c=.h)
EXTERNAL_HEADERS = $(EXTERNAL_SOURCES:.c=.h)

####### Implicit rules

.SUFFIXES:

.SUFFIXES: .c .o

.c.o:; $(CC) -c $(CFLAGS) $(MIPSFLAGS) $<

####### Build rules


ff: $(OBJECTS) $(PDDL_PARSER_OBJ)
	$(CC) -o mips-xxl $(OBJECTS) $(PDDL_PARSER_OBJ) $(CFLAGS) $(MIPSFLAGS) $(LIBS)

# pddl syntax

scan-xml.tab.c: scan-xml.y lex.xml.c
	bison -pxml -bscan-xml scan-xml.y	

scan-fct_pddl.tab.c: scan-fct_pddl.y lex.fct_pddl.c
	bison -pfct_pddl -bscan-fct_pddl scan-fct_pddl.y

scan-ops_pddl.tab.c: scan-ops_pddl.y lex.ops_pddl.c
	bison -pops_pddl -bscan-ops_pddl scan-ops_pddl.y


lex.fct_pddl.c: lex-fct_pddl.l
	flex -Pfct_pddl lex-fct_pddl.l

lex.ops_pddl.c: lex-ops_pddl.l
	flex -Pops_pddl lex-ops_pddl.l

lex.xml.c: lex-xml.l
	flex -Pxml lex-xml.l	


# misc
clean:
	rm -f $(OBJECTS) $(PDDL_PARSER_OBJ)

exclean:
	rm bucket.o duplicates_removal.o file.o external.o plan_reconstruction.o
backup:
	rm mips-xxlv3.tgz
	tar -czf mips-xxlv3.tgz $(EXTERNAL_SOURCES) $(EXTERNAL_HEADERS) 

veryclean: clean
	rm -f ff H* J* K* L* O* graph.* *.symbex gmon.out \
	$(PDDL_PARSER_SRC) \
	lex.fct_pddl.c lex.ops_pddl.c lex.xml.c lex.probname.c \
	*.output

depend:
	makedepend -- $(SOURCES) $(PDDL_PARSER_SRC)

lint:
	lclint -booltype Bool $(SOURCES) 2> output.lint

# DO NOT DELETE
