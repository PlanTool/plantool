
LIB	= .
INCLUDE = .
FLAGS	= -Wall -fPIC
CC	= gcc
#CPPFLAGS = -I$(INCLUDE) -g
CPPFLAGS = -I$(INCLUDE) -O3 -Wall

SRCS  = csp.c \
	debug.c \
	limit.c \
	stack.c \
	timer.c \
	solve_GAC.c \
	solve_GAC_CBJ.c \
	ebl.c

OBJS  =	$(SRCS:.c=.o)

all:	$(OBJS)

clean:	;rm -f $(OBJS)

###
csp.o:   global.h csp.h ebl.h
	$(CC) -c $(FLAGS) csp.c
	
debug.o: global.h csp.h
	$(CC) -c $(FLAGS) debug.c

limit.o: global.h
	$(CC) -c $(FLAGS) limit.c

stack.o: global.h
	$(CC) -c $(FLAGS) stack.c

solve_GAC.o: global.h csp.h stack.h
	$(CC) -c $(FLAGS) solve_GAC.c

solve_GAC_CBJ.o: global.h csp.h stack.h ebl.h
	$(CC) -c $(FLAGS) solve_GAC_CBJ.c

ebl.o: ebl.h global.h csp.h
	$(CC) -c $(FLAGS) ebl.c

timer.o:
	$(CC) -c $(FLAGS) timer.c
