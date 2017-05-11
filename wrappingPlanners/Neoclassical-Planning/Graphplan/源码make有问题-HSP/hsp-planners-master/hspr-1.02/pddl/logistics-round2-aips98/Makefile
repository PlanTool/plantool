AWK = awk
CC = gcc
CFLAGS = -I$(HSPRHOME)/planner -O3

PARSER = $(HSPRHOME)/parser/parser
PLANNER = $(HSPRHOME)/planner/planner.o

NO_PROFILED = $(PLANNER)
OBJS = $(NO_PROFILED)


#  Problems

$(PROBLEM:%.pddl=%):	$(DOMAIN) $(PROBLEM) $(OBJS) $(PARSER)
			-make dependencies PROBLEM=$(PROBLEM) DOMAIN=$(DOMAIN)
			-$(CC) -o $@ $(PROBLEM:%.pddl=%.ope.o) $(PROBLEM:%.pddl=%.fac.o) $(PROBLEM:%.pddl=%.tab.o) $(OBJS) -lm
			if test ! -x $(PROBLEM:%.pddl=%); \
			  then echo "$(PROBLEM:%.pddl=%) not built"; \
			  else echo "$(PROBLEM:%.pddl=%) built"; \
			fi

dependencies:		$(PROBLEM:%.pddl=%.ope.o) $(PROBLEM:%.pddl=%.fac.o) $(PROBLEM:%.pddl=%.tab.o)


$(PROBLEM:%.pddl=%.ope.o):	\
			$(PROBLEM:%.pddl=%.ope.c)
			$(CC) $(CFLAGS) -c $< $(DFLAGS)

$(PROBLEM:%.pddl=%.fac.o):	\
			$(PROBLEM:%.pddl=%.fac.c)
			$(CC) $(CFLAGS) -c $< $(DFLAGS)

$(PROBLEM:%.pddl=%.tab.o):	\
			$(PROBLEM:%.pddl=%.tab.c)
			$(CC) $(CFLAGS) -c $< $(DFLAGS)

$(PROBLEM:%.pddl=%.ope.c) $(PROBLEM:%.pddl=%.fac.c) $(PROBLEM:%.pddl=%.tab.c):	\
			$(PROBLEM) $(DOMAIN) $(PARSER)
			$(PARSER) $(DOMAIN) $(PROBLEM)
			mv xfile $(PROBLEM:%.pddl=%.ope.c)
			mv yfile $(PROBLEM:%.pddl=%.fac.c)
			mv zfile $(PROBLEM:%.pddl=%.tab.c)


#  Compile

compile:
			@echo "*** Compiling all in `pwd`"
			cat PROBLEMS | $(AWK) '{ system( "make PROBLEM=" $$1 " DOMAIN=" $$2 ); }'
			make clean




#  Solving

solve:
			@echo "*** Solving all in `pwd`"
			@echo "*** Output redirected to file \"solutions.all\""
			rm -f solutions.all
			cat PROBLEMS | $(AWK) -f $(HSPRHOME)/utils/solve.awk


#  Cleaning


clean:
			@echo "*** Cleaning `pwd`"
			rm -f *.o *.h *.c [xyz]file core gmon.out

clean-solve:
			@echo "*** Cleaning solutions in `pwd`"
			rm -f solutions.all

clean-problems:
			@echo "*** Cleaning all problems in `pwd`"
			find . -type f -perm -111 -exec rm -f {} \;

clean-all:		clean clean-solve clean-problems


# Fixed

$(PARSER):
			(cd $(HSPRHOME)/parser ; make $(PARSER))

$(PLANNER):		$(HSPRHOME)/planner/planner.c
			(cd $(HSPRHOME)/planner ; make $(PLANNER))

.SILENT:

