LIB   = .
INCLUDE = .
SHARE = ../Share

CC = gcc
#OPTFLAG = -O3
#OPTFLAG = -O2 -fnonnull-objects -fconserve-space 
#OPTFLAG = -g
CPPFLAGS = -O3 -Wall



OBJS = check.o gp_csp.o process.o generate.o hash.o

all: $(OBJS)

check.o: check.c gp_csp.h

gp_csp.o: gp_csp.c gp_csp.h

process.o: process.c gp_csp.h

generate.o: generate.c gp_csp.h

hash.o: hash.c gp_csp.h

clean:
	\rm -f *.o
