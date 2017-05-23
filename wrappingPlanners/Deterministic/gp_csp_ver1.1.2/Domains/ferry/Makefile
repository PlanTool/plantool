AWK = nawk
CC = gcc
CFLAGS = -I$(HSPHOME)/planner -g -Wall # -pg

PARSER = $(HSPHOME)/parser/parser
PLANNER = $(HSPHOME)/planner/planner.o
#PG_PLANNER = $(HSPHOME)/planner/pg_planner.o
COSTS = $(HSPHOME)/planner/costs.o
#PG_COSTS = $(HSPHOME)/planner/pg_costs.o
#GMON = /usr/local/gcc-2.7.2.3/arch/lib/gcc-lib/sparc-sun-solaris2.6/2.7.2.3/gmon.o

#PROFILED = $(PG_PLANNER) $(PG_COSTS) $(GMON)
NO_PROFILED = $(PLANNER) $(COSTS)

OBJS = $(NO_PROFILED)


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


#  All

all:
			@echo "*** Compiling all in `pwd`"
			cat PROBLEMS | $(AWK) '{ system( "make PROBLEM=" $$1 " DOMAIN=" $$2 ); }'
			make clean

all-prof:		all



#  Solving

solve:
			@echo "*** Solving all in `pwd`"
			rm -f solutions.temp
			cat PROBLEMS | $(AWK) -f $(HSPHOME)/utils/solve.awk seed=$(SEED) flags=$(FLAGS)
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
			@echo "*** Cleanning all problems in `pwd`"
			find . -type f -perm -111 -exec rm -f {} \;

clean-all:		clean clean-solve clean-problems


# Fixed

$(PARSER):
			(cd $(HSPHOME)/parser ; make $(PARSER))

$(PLANNER):		$(HSPHOME)/planner/planner.c
			(cd $(HSPHOME)/planner ; make $(PLANNER))

#$(PG_PLANNER):		$(HSPHOME)/planner/pg_planner.c
#			(cd $(HSPHOME)/planner ; make $(PG_PLANNER))

$(COSTS):		$(HSPHOME)/planner/costs.c
			(cd $(HSPHOME)/planner ; make $(COSTS))

#$(PG_COSTS):		$(HSPHOME)/planner/pg_costs.c
#			(cd $(HSPHOME)/planner ; make $(PG_COSTS))


.SILENT:

