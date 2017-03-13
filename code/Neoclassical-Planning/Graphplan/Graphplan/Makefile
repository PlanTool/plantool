CC	=gcc -g2
FLAGS	= -Wall -ansi

graphplan: y.tab.o lex.yy.o graphplan.o utilities.o hash.o planner.o dummy.o
	$(CC) $(FLAGS) lex.yy.o y.tab.o hash.o utilities.o graphplan.o planner.o dummy.o -o graphplan

y.tab.o: y.tab.h graphplan.h 


lex.yy.o: y.tab.h graphplan.h

hash.o: graphplan.h

graphplan.o: graphplan.h

utilities.o: graphplan.h

planner.o: graphplan.h

