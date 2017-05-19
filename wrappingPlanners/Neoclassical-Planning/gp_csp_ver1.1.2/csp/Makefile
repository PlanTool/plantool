LIB   = .
INCLUDE = .
SHARE = ../Share
FLAGS	= -Wall -fPIC

CC = gcc
#OPTFLAG = -O3
#OPTFLAG = -O2 -fnonnull-objects -fconserve-space 
#OPTFLAG = -g
CPPFLAGS = -O3 -Wall



OBJS = check.o gp_csp.o process.o generate.o hash.o

all: $(OBJS)

check.o: check.c gp_csp.h
	$(CC) -c $(FLAGS) check.c

gp_csp.o: gp_csp.c gp_csp.h
	$(CC) -c $(FLAGS) gp_csp.c

process.o: process.c gp_csp.h
	$(CC) -c $(FLAGS) process.c

generate.o: generate.c gp_csp.h
	$(CC) -c $(FLAGS) generate.c

hash.o: hash.c gp_csp.h
	$(CC) -c $(FLAGS) hash.c

clean:
	\rm -f *.o
