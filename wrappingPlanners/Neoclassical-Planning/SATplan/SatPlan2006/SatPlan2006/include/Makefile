#!/bin/sh
#
# Makefile for FF v 1.0
#


####### FLAGS

TYPE	= 
ADDONS	= 

CC      = gcc

CFLAGS	= -O6 -Wall -ansi -static $(TYPE) $(ADDONS) -g
#-g -pg

LIBS    = -lm

CPP     = g++

CPPFLAGS = -ansi -Wall -pedantic -static


####### Files

PDDL_PARSER_SRC	= scan-fct_pddl.tab.c \
	scan-ops_pddl.tab.c \
	scan-probname.tab.c \
	lex.fct_pddl.c \
	lex.ops_pddl.c 

PDDL_PARSER_OBJ = scan-fct_pddl.tab.o \
	scan-ops_pddl.tab.o 


SOURCES = main.c \
	memory.c \
	output.c \
	parse.c \
	instantiateI.c \
	instantiateII.c \
	graph.c \
	cnfout.c

CPP_SOURCES = SatPlan.cpp \
	      StringAlgorithms.cpp \
              Input.cpp \
              SolverInterface.cpp \
                GeneralSolver.cpp \
                SiegeSolver.cpp \
                SatzRandSolver.cpp \
                Jerusat-1.3.cpp \
                BerkMin561.cpp \
                MiniSatSolver.cpp

OBJECTS = $(SOURCES:.c=.o)

CPPOBJECTS = $(CPP_SOURCES:.cpp=.o)

####### Implicit rules

.SUFFIXES:

.SUFFIXES: .c .o

.SUFFIXES: .cpp .o

.c.o:; $(CC) -c $(CFLAGS) $<

.cpp.o:; $(CPP) -c $(CPPFLAGS) $<

####### Build rules

all: bb satplan

satplan: $(CPPOBJECTS)
	$(CPP) -o satplan $(CPPOBJECTS) $(CPPFLAGS)

bb: $(OBJECTS) $(PDDL_PARSER_OBJ) 
	$(CC) -o bb $(OBJECTS) $(PDDL_PARSER_OBJ) $(CFLAGS) $(LIBS)

# pddl syntax
scan-fct_pddl.tab.c: scan-fct_pddl.y lex.fct_pddl.c
	bison -pfct_pddl -bscan-fct_pddl scan-fct_pddl.y

scan-ops_pddl.tab.c: scan-ops_pddl.y lex.ops_pddl.c
	bison -pops_pddl -bscan-ops_pddl scan-ops_pddl.y

lex.fct_pddl.c: lex-fct_pddl.l
	flex -Pfct_pddl lex-fct_pddl.l

lex.ops_pddl.c: lex-ops_pddl.l
	flex -Pops_pddl lex-ops_pddl.l


####### CPP Dependencies
GENERALFILES = Assertion.h ByLine.h CommonErrors.h Conversion.h Exceptions.h \
               Input.h StandardFiles.h StringAlgorithms.h Timer.h Typify.h

SOLVERFILES =  Factory.h SingletonType.h SolverInterface.h SolverTraits.h 


BerkMin561.o: $(GENERALFILES) $(SOLVERFILES) BerkMin561.h

GeneralSolver.o: $(GENERALFILES) $(SOLVERFILES) GeneralSolver.h

Input.o: $(GENERALFILES) $(SOLVERFILES) PreferredSource.h

Jerusat-1.3.o: $(GENERALFILES) $(SOLVERFILES) Jerusat-1.3.h

MiniSatSolver.o: $(GENERALFILES) $(SOLVERFILES) MiniSatSolver.h

SatzRandSolver.o: $(GENERALFILES) $(SOLVERFILES) SatzRandSolver.h

SiegeSolver.o: $(GENERALFILES) $(SOLVERFILES) SiegeSolver.h

SatPlan.o: $(GENERALFILES) $(SOLVERFILES) CNFEncoding.h PreferredSource.h

SolverInterface.o: $(GENERALFILES) $(SOLVERFILES)

StringAlgorithms.o: StandardFiles.h StringAlgorithms.h



# misc
clean:
	rm -f satplan bb *.o *.bak *~ *% core *_pure_p9_c0_400.o.warnings \
        \#*\# $(RES_PARSER_SRC)
##$(PDDL_PARSER_SRC)

veryclean: clean
	rm -f satplan bb *.symbex gmon.out \
##$(PDDL_PARSER_SRC) \
	lex.fct_pddl.c lex.ops_pddl.c lex.probname.c \
	*.output DATA CNF

depend:
	makedepend -- $(SOURCES) $(CPP_SOURCES) $(PDDL_PARSER_SRC)

lint:
	lclint -booltype Bool $(SOURCES) 2> output.lint

# DO NOT DELETE
