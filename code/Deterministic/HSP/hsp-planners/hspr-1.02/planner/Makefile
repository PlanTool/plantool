CC = gcc
CFLAGS = -O3


compile:
			@echo "*** Compiling all in `pwd`"
			@make planner.o


planner.o:		planner.c planner.h
			@$(CC) $(CFLAGS) -c planner.c

clean:
			@echo "*** Cleaning all in `pwd`"
			@rm -f *.o

.SILENT:

