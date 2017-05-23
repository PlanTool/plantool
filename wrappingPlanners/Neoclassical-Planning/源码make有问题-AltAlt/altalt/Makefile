CC      = gcc 
CCC 	= g++
FLAGS	= -O3 -ansi
CFLAGS = -g -Wall

altalt: pddl.yacc.tab.o Rules.o States.o SymTab.o BasicTim.o TimInterface.o globals.o facts.o actions.o instantiation.o construction.o badgoals.o searchhsp.o candidates.o search.o main.o
	$(CCC) $(FLAGS)  pddl.yacc.tab.o SymTab.o Rules.o States.o BasicTim.o TimInterface.o globals.o facts.o actions.o instantiation.o construction.o badgoals.o searchhsp.o candidates.o search.o main.o -lfl -o altalt 

lex.yy.c: pddl.lex
	flex pddl.lex

pddl.yacc.tab.c: alldefs.h pddl.yacc lex.yy.c
	bison pddl.yacc

pddl.yacc.tab.o: lex.yy.c pddl.yacc.tab.c
	$(CC) $(FLAGS) pddl.yacc.tab.c -c

globals.o: switches.h alldefs.h globals.h globals.cc
	$(CCC) $(FLAGS) globals.cc -c

facts.o: facts.h alldefs.h actions.h globals.h stan.h facts.cc
	$(CCC) $(FLAGS) facts.cc -c

actions.o: switches.h facts.h actions.h globals.h alldefs.h actions.cc
	$(CCC) $(FLAGS) actions.cc -c

instantiation.o: switches.h alldefs.h instantiation.h globals.h facts.h actions.h stan.h instantiation.cc
	$(CCC) $(FLAGS) instantiation.cc -c

construction.o: switches.h alldefs.h globals.h construction.h actions.h facts.h construction.cc
	$(CCC) $(FLAGS) construction.cc -c
 
badgoals.o: badgoals.h candidates.h search.h alldefs.h globals.h actions.h facts.h badgoals.cc
	$(CCC) $(FLAGS) badgoals.cc -c

searchhsp.o: searchhsp.h alldefs.h globals.h facts.h actions.h badgoals.h candidates.h construction.h searchhsp.cc
	$(CC) $(CFLAGS) -c searchhsp.cc

candidates.o: switches.h alldefs.h globals.h facts.h actions.h search.h candidates.h candidates.cc
	$(CCC) $(FLAGS) candidates.cc -c

search.o: switches.h alldefs.h search.h globals.h actions.h facts.h badgoals.h candidates.h construction.h searchhsp.h search.cc
	$(CCC) $(FLAGS) search.cc -c

main.o: switches.h alldefs.h globals.h facts.h actions.h instantiation.h construction.h search.h badgoals.h candidates.h TimInterface.h SymTab.h BasicTim.h main.cc
	$(CCC) $(FLAGS) main.cc -c

tim: SymTab.o BasicTim.o Rules.o States.o tim.o TimInterface.o pddl.yacc.tab.o
	$(CCC) $(FLAGS) pddl.yacc.tab.o SymTab.o BasicTim.o Rules.o States.o tim.o TimInterface.o -lfl -o tim

tim.o: alldefs.h States.h SymTab.h BasicTim.h switches.h Rules.h TimInterface.h tim.cc
	$(CCC) $(FLAGS) tim.cc -c


TimInterface.o: States.h SymTab.h TimInterface.h switches.h Rules.h BasicTim.h TimInterface.cc
	$(CCC) $(FLAGS) TimInterface.cc -c

BasicTim.o: alldefs.h SymTab.h BasicTim.h switches.h BasicTim.cc
	$(CCC) $(FLAGS) BasicTim.cc -c

SymTab.o: SymTab.h switches.h SymTab.cc
	$(CCC) $(FLAGS) SymTab.cc -c

States.o: States.h Rules.h BasicTim.h SymTab.h switches.h States.cc
	$(CCC) $(FLAGS) States.cc -c

Rules.o: Rules.h BasicTim.h SymTab.h States.h switches.h Rules.cc
	$(CCC) $(FLAGS) Rules.cc -c


clean:
	rm -f *.o core stan tim pddl.yacc.* lex.* *~ *%

