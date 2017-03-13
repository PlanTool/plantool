AWK = awk
CC = gcc
CFLAGS = -I$(HSPHOME)/planner -O3

PARSER = $(HSPHOME)/parser/parser
PLANNER = $(HSPHOME)/planner/planner.o
COSTS = $(HSPHOME)/planner/costs.o
OBJS = $(PLANNER) $(COSTS)


#  Problems

$(PROBLEM:%.pddl=%):	$(DOMAIN) $(PROBLEM) $(OBJS) $(PARSER)
			-make dependencies PROBLEM=$(PROBLEM) DOMAIN=$(DOMAIN)
			-$(CC) -o $@ $(PROBLEM:%.pddl=%.ope.o) $(PROBLEM:%.pddl=%.fac.o) $(PROBLEM:%.pddl=%.tab.o) $(OBJS)
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


#  compile

compile:
			@echo "*** Compiling all in `pwd`"
			cat PROBLEMS | $(AWK) '{ system( "make PROBLEM=" $$1 " DOMAIN=" $$2 ); }'
			make clean


#  Solving

solve:
			@echo "*** Solving all in `pwd`"
			rm -f solutions.temp
			cat PROBLEMS | $(AWK) -f $(HSPHOME)/utils/solve.awk seed=$(HSPSEED) flags=$(HSPFLAGS)
			cat solutions.temp | $(AWK) -f $(HSPHOME)/utils/post.awk > solutions.all
			rm -f solutions.temp


#  Cleanning


clean:
			@echo "*** Cleanning `pwd`"
			rm -f *.o *.h *.c [xyz]file core

clean-solve:
			@echo "*** Cleanning solutions in `pwd`"
			rm -f solutions.all solutions.temp

clean-problems:
			@echo "*** Cleanning problems in `pwd`"
			find . -type f -perm -111 -exec rm -f {} \;

clean-all:		clean clean-solve clean-problems


# Fixed

$(PARSER):
			(cd $(HSPHOME)/parser ; make $(PARSER))

$(PLANNER):		$(HSPHOME)/planner/planner.c
			(cd $(HSPHOME)/planner ; make $(PLANNER))

$(COSTS):		$(HSPHOME)/planner/costs.c
			(cd $(HSPHOME)/planner ; make $(COSTS))

.SILENT:

