CC	=gcc -g2
FLAGS	= -Wall -ansi -fPIC

SOURCES 	= y.tab.c \
	lex.yy.c \
	graphplan.c \
	utilities.c \
	hash.c \
	planner.c \
	dummy.c


OBJECTS 	= $(SOURCES:.c=.o)

INC_PYTHON = -I/usr/include/python2.7

_graphplan.so: $(OBJECTS) $(PDDL_PARSER_OBJ) graphplan_wrap.o
		gcc -shared -o $@ $^

graphplan: y.tab.o lex.yy.o graphplan.o utilities.o hash.o planner.o dummy.o
	$(CC) $(FLAGS) lex.yy.o y.tab.o hash.o utilities.o graphplan.o planner.o dummy.o -o graphplan

graphplan_wrap.o: graphplan_wrap.c
		$(CC) -c $(INC_PYTHON) $(FLAGS) $^

graphplan.py graphplan_wrap.c: graphplan.i
				swig -python graphplan.i

y.tab.o: y.tab.h graphplan.h 
	$(CC) -c $(FLAGS) y.tab.c

lex.yy.o: y.tab.h graphplan.h
	$(CC) -c $(FLAGS) lex.yy.c

hash.o: graphplan.h
	$(CC) -c $(FLAGS) hash.c

graphplan.o: graphplan.h
	$(CC) -c $(FLAGS) graphplan.c

utilities.o: graphplan.h
	$(CC) -c $(FLAGS) utilities.c

planner.o: graphplan.h
	$(CC) -c $(FLAGS) planner.c

dummy.o: 
	$(CC) -c $(FLAGS) dummy.c