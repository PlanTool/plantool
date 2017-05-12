OBJ	=	actions.o algorithms.o atom_states.o client.o domains.o effects.o \
		exceptions.o expressions.o formulas.o functions.o global.o graph.o hash.o \
		heuristics.o main.o md4c.o parser.o lexer.o planners.o predicates.o \
		problems.o requirements.o strxml.o terms.o types.o utils.o

DEFS	=	-DNO_STRICT
#DEFS	=	-DMEM_DEBUG -DATOM_STATES -DNO_STRICT
ODEFS	=	-DNDEBUG -DNO_STRICT

LDFLAGS	=	#-R /opt/sfw/gcc-3.4.1/lib
#CFLAGS	=	-Wno-deprecated -g -Wall $(DEFS)
# COMMENT OUT THE LINE BELOW AND UNCOMMENT THE LINE ABOVE TO COMPILE IN DEBUG MODE W/O OPTIMIZATIONS
CFLAGS	=	-Wno-deprecated -O3 -s -Wall $(ODEFS)
LIBS	=	-lnsl -lpthread
CC	=	gcc
C++	=	g++
YACC	=	/usr/bin/bison -d
LEX	=	/usr/bin/flex


planner:	$(OBJ)
		$(C++) -o planner $(OBJ) $(LDFLAGS) $(LIBS)

pplanner:	$(OBJ)
		purify $(C++) -o pplanner $(OBJ) $(LDFLAGS) $(LIBS)

parser.cc:	parser.y
		$(YACC) parser.y
		mv parser.tab.c parser.cc
		mv parser.tab.h parser.h

lexer.cc:	lexer.l
		$(LEX) lexer.l
		mv lex.yy.c lexer.cc

.c.o:
		$(CC) $(CFLAGS) $(INCLUDE) -c $<

.cc.o:
		$(C++) $(CFLAGS) $(INCLUDE) -c $<

clean:
		rm -f *.o *~

dist:
		tar -cvf - Makefile.* report.awk code.scm *.c *.cc *.h *.y *.l | gzip -c > ../dist.tgz

# DO NOT DELETE

actions.o: actions.h global.h effects.h terms.h types.h hashing.h formulas.h
actions.o: predicates.h  utils.h exceptions.h problems.h domains.h
actions.o: functions.h requirements.h expressions.h states.h
algorithms.o: hash.h global.h heuristics.h actions.h effects.h terms.h
algorithms.o: types.h hashing.h formulas.h predicates.h  utils.h
algorithms.o: states.h md4.h problems.h domains.h functions.h requirements.h
algorithms.o: expressions.h queue.h algorithms.h
atom_states.o: global.h domains.h actions.h effects.h terms.h types.h
atom_states.o: hashing.h formulas.h predicates.h  utils.h
atom_states.o: functions.h requirements.h expressions.h hash.h heuristics.h
atom_states.o: states.h md4.h problems.h
client.o: global.h client.h  strxml.h domains.h actions.h effects.h
client.o: terms.h types.h hashing.h formulas.h predicates.h utils.h
client.o: functions.h requirements.h exceptions.h hash.h heuristics.h
client.o: states.h md4.h planners.h algorithms.h
domains.o: domains.h actions.h global.h effects.h terms.h types.h hashing.h
domains.o: formulas.h predicates.h  utils.h functions.h
domains.o: requirements.h problems.h expressions.h
effects.o: global.h actions.h effects.h terms.h types.h hashing.h formulas.h
effects.o: predicates.h  utils.h problems.h domains.h functions.h
effects.o: requirements.h expressions.h exceptions.h states.h
exceptions.o: exceptions.h
expressions.o: expressions.h functions.h types.h hashing.h terms.h global.h
expressions.o:  problems.h actions.h effects.h formulas.h
expressions.o: predicates.h utils.h domains.h requirements.h exceptions.h
formulas.o: domains.h actions.h global.h effects.h terms.h types.h hashing.h
formulas.o: formulas.h predicates.h  utils.h functions.h
formulas.o: requirements.h exceptions.h expressions.h problems.h states.h
formulas.o: strxml.h
functions.o: functions.h types.h hashing.h
global.o: global.h
graph.o: graph.h global.h utils.h
hash.o: global.h actions.h effects.h terms.h types.h hashing.h formulas.h
hash.o: predicates.h  utils.h problems.h domains.h functions.h
hash.o: requirements.h expressions.h hash.h heuristics.h states.h md4.h
heuristics.o: global.h actions.h effects.h terms.h types.h hashing.h
heuristics.o: formulas.h predicates.h  utils.h algorithms.h hash.h
heuristics.o: heuristics.h states.h md4.h exceptions.h queue.h
lexer.o: effects.h global.h terms.h types.h hashing.h formulas.h predicates.h
lexer.o:  parser.h
main.o: global.h actions.h effects.h terms.h types.h hashing.h formulas.h
main.o: predicates.h  utils.h client.h strxml.h domains.h
main.o: functions.h requirements.h exceptions.h planners.h algorithms.h
main.o: hash.h heuristics.h states.h md4.h problems.h expressions.h
parser.o: global.h problems.h actions.h effects.h terms.h types.h hashing.h
parser.o: formulas.h predicates.h  utils.h domains.h functions.h
parser.o: requirements.h expressions.h exceptions.h
planners.o: global.h hash.h heuristics.h actions.h effects.h terms.h types.h
planners.o: hashing.h formulas.h predicates.h  utils.h states.h
planners.o: md4.h planners.h algorithms.h
predicates.o: predicates.h types.h hashing.h
problems.o: problems.h global.h actions.h effects.h terms.h types.h hashing.h
problems.o: formulas.h predicates.h  utils.h domains.h functions.h
problems.o: requirements.h expressions.h exceptions.h graph.h states.h
requirements.o: requirements.h
strxml.o: global.h strxml.h
terms.o: terms.h global.h types.h hashing.h
types.o: global.h types.h hashing.h
utils.o: states.h utils.h global.h md4.h
md4c.o: md4.h
mutex.o: parser.h
