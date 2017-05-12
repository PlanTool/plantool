#CC = CC
#CC = g++-3.2
#CC=g++ -Wall -O3
CC=g++ -O3
#CC=g++ -Wall -pg -O3
#CC=g++ -Wall -ggdb -O3
#CFLAGS = -O
#CFLAGS = -Wall -g 
#CFLAGS = -O -g 
#CFLAGS = -O3 -pg 
#CFLAGS = -O3
#CFLAGS = -Wall -O3
CFLAGS = -DNDEBUG -O3 

MFLAGS = 
LINKFLAGS = --static

ifeq (solaris, $(OSTYPE))
  MFLAGS = -D_NEED_REDEFINE_RAND_MAX_
endif

RANLIB = ranlib
AR = ar

.SUFFIXES: .o .cpp 

HEADERS = zchaff_base.h zchaff_clsgen.h zchaff_header.h zchaff_version.h zchaff_dbase.h zchaff_solver.h

SOLVER_SRCS = sat_solver.cpp 
SOLVER_OBJS = $(SOLVER_SRCS:.cpp=.o)

LIB_SRCS =  zchaff_utils.cpp \
	    zchaff_solver.cpp\
	    zchaff_base.cpp \
	    zchaff_dbase.cpp \
	    zchaff_c_wrapper.cpp \
	    zchaff_cpp_wrapper.cpp \

LIB_OBJS = $(LIB_SRCS:.cpp=.o)


zchaff:   $(SOLVER_OBJS) libsat.a SAT_C.h
	  $(CC) $(LINKFLAGS) $(CFLAGS) $(MFLAGS) $(SOLVER_OBJS) libsat.a -o zchaff 

zverify_bf: zverify_bf.cpp	
	  $(CC) $(LINKFLAGS) $(CFLAGS) $(MFLAGS) zverify_bf.cpp -o zverify_bf

zverify_df: zverify_df.cpp
	  $(CC) $(LINKFLAGS) $(CFLAGS) $(MFLAGS) zverify_df.cpp -o zverify_df

zcore: zcore_extract.cpp
	  $(CC) $(LINKFLAGS) $(CFLAGS) $(MFLAGS) zcore_extract.cpp -o zcore

zminimal: zminimal.cpp libsat.a
	  $(CC) $(LINKFLAGS) $(CFLAGS) $(MFLAGS) zminimal.cpp libsat.a -o zminimal

cnf_stats: cnf_stats.cpp
	  $(CC) $(LINKFLAGS) $(CFLAGS) $(MFLAGS) cnf_stats.cpp -o cnf_stats

$(LIB_OBJS): $(HEADERS) Makefile

zchaff_c_wrapper.cpp:	zchaff_wrapper.wrp
		sed 's/EXTERN/extern \"C\"/' zchaff_wrapper.wrp > zchaff_c_wrapper.cpp

zchaff_cpp_wrapper.cpp:	zchaff_wrapper.wrp
		sed 's/EXTERN//' zchaff_wrapper.wrp > zchaff_cpp_wrapper.cpp
SAT_C.h:
	sed 's/gid = 0/gid/' SAT.h > SAT_C.h

libsat.a:   $(LIB_OBJS)
	@rm -f libsat.a
	$(AR) cr libsat.a $(LIB_OBJS)
	$(RANLIB) libsat.a

.cpp.o:
	$(CC) $(CFLAGS) $(MFLAGS) -c $< 

clean:	
	rm -f *.o libsat.a zchaff *wrapper.cpp zminimal zcore zverify_bf zverify_df cnf_stats SAT_C.h

all: zchaff zverify_bf zverify_df zcore zminimal cnf_stats
	 	  




