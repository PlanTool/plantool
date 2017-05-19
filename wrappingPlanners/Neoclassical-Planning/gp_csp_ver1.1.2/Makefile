#CC = g++ -g2 -O2 -fconserve-space -fnonnull-objects -Wall
CC	= gcc -g2
PFLAGS = -lfl
FLAGS	= -Wall -fPIC

INC_PYTHON = -I/usr/include/python2.7

#OBJS = y.tab.o lex.yy.o graphplan.o utilities.o hash.o planner.o dummy.o 
OBJS = bbpddl.tab.o lex.yy.o graphplan.o utilities.o hash.o planner.o dummy.o justify.o
COBJS = csp/check.o csp/generate.o csp/gp_csp.o csp/hash.o csp/process.o
SOBJS = Share/csp.o Share/debug.o Share/limit.o Share/stack.o Share/timer.o Share/solve_GAC_CBJ.o Share/ebl.o
 

_gpcsp.so: $(OBJS)  graphplan_wrap.o
		cd csp; make
		cd Share; make
		gcc -shared $(COBJS) $(SOBJS) -o $@ $^ $(PFLAGS)

graphplan_wrap.o: graphplan_wrap.c
		$(CC) -c $(INC_PYTHON) $(FLAGS) $^

gpcsp.py graphplan_wrap.c: graphplan.i
				swig -python graphplan.i

gpcsp: $(OBJS)
	cd csp; make
	cd Share; make
	$(CC) $(FLAGS) $(OBJS) $(PFLAGS) $(COBJS) $(SOBJS) -o gpcsp 

#y.tab.o: y.tab.h graphplan.h 
bbpddl.tab.c: bbpddl.y lex.yy.c
	bison -d bbpddl.y

bbpddl.tab.o: bbpddl.tab.c graphplan.h
	$(CC) -c $(FLAGS) bbpddl.tab.c 

lex.yy.c: bbpddl.l
	flex bbpddl.l

#lex.yy.o: y.tab.h graphplan.h
lex.yy.o: graphplan.h bbpddl.tab.h
	$(CC) -c $(FLAGS) lex.yy.c

hash.o: graphplan.h
	$(CC) -c $(FLAGS) hash.c

graphplan.o: graphplan.h
	$(CC) -c $(FLAGS) graphplan.c

utilities.o: graphplan.h
	$(CC) -c $(FLAGS) utilities.c

planner.o: graphplan.h
	$(CC) -c $(FLAGS) planner.c

justify.o: justify.h graphplan.h
	$(CC) -c $(FLAGS) justify.c

dummy.o: 
	$(CC) -c $(FLAGS) dummy.c

clean:
	cd csp; make clean
	cd Share; make clean
	/bin/rm *.o lex*.c *.tab.c gpcsp *.so
