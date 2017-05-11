CC = gcc
CFLAGS = -O3


compile:
			@echo "*** Compiling all in `pwd`"
			@make planner.o
			@make costs.o


planner.o:		planner.c planner.h
			@$(CC) $(CFLAGS) -c planner.c

costs.o:		costs.c planner.h
			@$(CC) $(CFLAGS) -c costs.c


clean:
			@echo "*** Cleanning all in `pwd`"
			@rm -f *.o

.SILENT:

