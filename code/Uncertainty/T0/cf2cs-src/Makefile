

# For non-optimizing, comment both
C_OPT	= -O3
OC_OPT	= -ccopt -O3 -w A -dtypes -g #-inline 100 
#OC_OPT	=  -w A -dtypes -g
#OC_OPT	= -ccopt -O3 -inline 1000 -ffast-math -unsafe

# Uncomment for profiling, both
#PROFILE_C = -pg -O
#PROFILE_OCAML = -g  -p

# Uncomment for debugging from glibc
#CHECK=-D_GLIBCXX_DEBUG -D_GLIBCXX_CONCEPT_CHECKS -D_GLIBCXX_DEBUG_PEDANTIC

# Uncomment for mem debugging
#EFENCE = -lefence
#OC_EFENCE = -ccopt -lefence

# Uncomment for fast & dirty
#NDEBUG=-DNDEBUG

# Uncomment for Fix architecture
#ARCH=-march=pentium-m 

# END OF USER-FIX

CC	= gcc $(ARCH) $(NDEBUG)
C++     = g++ $(ARCH) $(NDEBUG)
CC	= $(C++)
YACC	= yacc
FLEX	= flex

OCAMLC=ocamlopt.opt
OCAMLOPT=ocamlopt.opt
# If opt version are not available:
#OCAMLC=ocamlc
#OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
RES_DIR=res-2.2.5
OCAMLCOMMON=-I $(RES_DIR)/lib $(OC_OPT) $(PROFILE_OCAML) -verbose 
OCAMLFLAGS=$(OCAMLCOMMON)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(OCAMLCOMMON) \
	-ccopt -lnnf -ccopt -lstdc++ -verbose $(OC_EFENCE) -ccopt -L$(NNF) \
	-ccopt -L$(HMETIS) -ccopt -lnnf -ccopt -lhmetis 
# add other options for ocamlopt here

GDB=-ggdb3
ZCHAFF	= zChaff
NNF	= nnf
HMETIS	= hmetis-1.5-linux
OCAMLDIR= /usr/lib/ocaml
# Put here your local instalation of OCaml
#OCAMLDIR= /home/hlp/local/lib/ocaml
CFLAGS	= -Wall -DUSE_CLAUSE $(GDB) -I$(NNF) -I$(ZCHAFF) $(PROFILE_C) $(CHECK) $(C_OPT) $(EFENCE)\
-I$(OCAMLDIR) -fno-stack-protector 
# -fstack-protector is added by default in my
# gcc 4.1 under Ubuntu 7.10, but then it fails in other OS as Rock Cluster

SOURCES_C=`cat SOURCES`
SOURCES_ML=cf2cs.ml
SOURCESALL=$(SOURCES_C) $(SOURCES_ML) 

all: cf2cs prime-impl

cf2cs:  parser.o pddl.o clauses.o main.o cf2cs-ocaml.o $(NNF)/libnnf.a cf2cs.cmx $(RES_DIR)/lib/res.cmxa
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o cf2cs res.cmxa unix.cmxa str.cmxa $^ 

prime-impl: prime-impl-test.cc prime-impl.o clauses.o
	$(C++) $(CFLAGS) -o prime-impl prime-impl-test.cc prime-impl.o clauses.o

$(RES_DIR)/lib/res.cmi $(RES_DIR)/lib/res.cmxa:
	@cd $(RES_DIR) && make OCAMLLIBPATH=$(OCAMLDIR)

cf2cs.cmx: $(RES_DIR)/lib/res.cmi

$(NNF)/libnnf.a:
	@cd $(NNF) && make libnnf.a

tokens.h parser.cc:	parser.y
			$(YACC) -v -d -t parser.y
			mv y.tab.c parser.cc
			mv y.tab.h tokens.h

pddl.cc:		pddl.lex
			$(FLEX) -i -t pddl.lex > pddl.cc

.SUFFIXES: .c .cc .ml .cmx .o
.c.o:
	$(CC) $(CFLAGS) -c $<
.cc.o:
	$(CC) $(CFLAGS) -c $<
.ml.o:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.ml.cmx:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

etags: 
	etags --members $(SOURCES_C)

# do a "co RCS/*" before make depend, so all files are available
depend:
			makedepend -- -Y $(CFLAGS) -- $(SOURCES_C)

clean:
			rm -f core cf2cs *.o *.gch y.output *~ pddl.cc parser.cc tokens.h prime-impl *.cm* *.annot
			cd test-cf2cs; ./clean-trans

full-clean: clean
	@cd $(RES_DIR) && make clean
	@cd $(NNF) && make clean

gz:			all
			gzip cf2cs
			scp cf2cs.gz hpalacio@rigel.upf.edu:
			gunzip cf2cs.gz


t0s:			all
			@echo ==========================================================================
			@echo -n "Testing t0 .... "
			test-cf2cs/test-t0

t0:			all
			@echo ==========================================================================
			@echo -n "Testing t0 and clean .... "
			test-cf2cs/test-t0
			cd test-cf2cs; ./clean-trans

s0:			all
			@echo ==========================================================================
			@echo -n "Testing s0.... "
			test-cf2cs/test-s0


parser.o: clauses.hpp

# DO NOT DELETE

prime-impl-test.o: prime-impl.hpp clauses.hpp
prime-impl.o: prime-impl.hpp clauses.hpp
cf2cs-ocaml.o: cf2cs-ocaml.hpp nnf/nnf.h nnf/cnf.h nnf/strxml.h
cf2cs-ocaml.o: zChaff/zchaff_solver.h zChaff/zchaff_version.h
cf2cs-ocaml.o: zChaff/zchaff_dbase.h zChaff/zchaff_base.h
cf2cs-ocaml.o: zChaff/zchaff_header.h nnf/hashing.h planner.h parser.h
cf2cs-ocaml.o: clauses.hpp parser-global.hpp main.hpp
clauses.o: clauses.hpp
main.o: nnf/nnf.h nnf/cnf.h nnf/strxml.h zChaff/zchaff_solver.h
main.o: zChaff/zchaff_version.h zChaff/zchaff_dbase.h zChaff/zchaff_base.h
main.o: zChaff/zchaff_header.h nnf/hashing.h cf2cs-ocaml.hpp planner.h
main.o: parser.h clauses.hpp parser-global.hpp main.hpp
parser.o: nnf/nnf.h nnf/cnf.h nnf/strxml.h zChaff/zchaff_solver.h
parser.o: zChaff/zchaff_version.h zChaff/zchaff_dbase.h zChaff/zchaff_base.h
parser.o: zChaff/zchaff_header.h nnf/hashing.h parser.h planner.h clauses.hpp
pddl.o: parser.h tokens.h
planner.o: parser.h
main.o: planner.h parser.h
cf2cs-ocaml.o: nnf/nnf.h nnf/cnf.h nnf/strxml.h zChaff/zchaff_solver.h
cf2cs-ocaml.o: zChaff/zchaff_version.h zChaff/zchaff_dbase.h
cf2cs-ocaml.o: zChaff/zchaff_base.h zChaff/zchaff_header.h nnf/hashing.h
cf2cs-ocaml.o: planner.h parser.h
