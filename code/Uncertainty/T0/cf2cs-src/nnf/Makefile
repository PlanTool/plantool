SRCS	= cnf.cc dtree.cc hgraph.cc md4c.c nnf.cc satlib.cc strxml.cc satlib-main.cc nnf-main.cc
OBJS	= cnf.o dtree.o hgraph.o md4c.o nnf.o satlib.o strxml.o
NNFOBJS	= nnf.o satlib.o strxml.o
ALLOBJS	= $(OBJS) satlib-main.o nnf-main.o
#CUDD	= ../cudd-2.3.1
#CS	= ../kres
#HMETIS	= ../../hmetis-1.5-linux
HMETIS	= ../hmetis-1.5-linux
#HMETIS	= ../../hmetis-1.5-sun4u-USparc
ZCHAFF	= ../zChaff
#ZCHAFF	= .
SATLIB	= sat
MAKEDEP	= makedepend	
#MAKEDEP	= /usr/openwin/bin/makedepend	

INCLUDE = -I$(HMETIS) -I$(ZCHAFF) #-I$(CS) -I$(CUDD)/include
#CFLAGS	= -g -Wall -DDEBUG -DPURE #-DCONFLICT_DRIVEN_BACKTRACKING #-DZBDD
CFLAGS	= -O3 -Wall -DNNF_ONLY -DNDEBUG -DPURE #-DCONFLICT_DRIVEN_BACKTRACKING #-DNBEDUG #-DZBDD
LDFLAGS	= -lnnf -lhmetis -l$(SATLIB)
LIBS	= -L$(HMETIS) -L$(ZCHAFF) -L.
CC	= gcc
C++     = g++
EXTRA	= #-pg

satlib:		satlib-main.o libnnf.a $(ZCHAFF)/lib$(SATLIB).a
		$(C++) $(EXTRA) -o satlib satlib-main.o $(LIBS) $(LDFLAGS)

$(ZCHAFF)/lib$(SATLIB).a:
	@cd $(ZCHAFF) && make lib$(SATLIB).a

nnf:		nnf-main.o libnnf.a
		$(C++) $(EXTRA) -o nnf nnf-main.o $(LIBS) $(LDFLAGS)

libnnf.a:	$(OBJS)
		ar rv libnnf.a $(OBJS)

.cc.o:
		$(C++) $(EXTRA) $(CFLAGS) -c $< $(INCLUDE)

.c.o:
		$(CC) $(EXTRA) $(CFLAGS) -c $< $(INCLUDE)

backup:
		tar -cvf - . | gzip -c > ../nnf.tar.gz

clean:
		rm -f satlib nnf libnnf.a $(ALLOBJS) *~ core
		@cd $(ZCHAFF) && make clean

dep:
		$(MAKEDEP) -Y $(SRCS)

# DO NOT DELETE

cnf.o: cnf.h strxml.h dtree.h nnf.h hashing.h hgraph.h md4.h
dtree.o: cnf.h strxml.h dtree.h nnf.h hashing.h satlib.h
hgraph.o: cnf.h strxml.h dtree.h nnf.h hashing.h hgraph.h
md4c.o: md4.h
nnf.o: nnf.h cnf.h strxml.h hashing.h
satlib.o: cnf.h strxml.h nnf.h hashing.h satlib.h
strxml.o: strxml.h
satlib-main.o: cnf.h strxml.h dtree.h nnf.h hashing.h hgraph.h satlib.h
nnf-main.o: nnf.h cnf.h strxml.h hashing.h satlib.h
