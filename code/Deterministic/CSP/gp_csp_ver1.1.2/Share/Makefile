
LIB	= .
INCLUDE = .

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
debug.o: global.h csp.h
limit.o: global.h
stack.o: global.h
solve_GAC.o: global.h csp.h stack.h
solve_GAC_CBJ.o: global.h csp.h stack.h ebl.h
ebl.o: ebl.h global.h csp.h
