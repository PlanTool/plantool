BINARY = cachet

DFLAGS = -DAPPROXIMATE_HASHING#-DBIG_NUM
LFLAGS = #-lgmp
SOPT = #big_num.cpp

CC = g++
CFLAGS = -O6 -DNDEBUG #-pg

RANLIB = ranlib
AR = ar

.SUFFIXES: .o .cpp .c

HEADERS = zchaff_base.h zchaff_header.h zchaff_dbase.h zchaff_solver.h hash.h big_num.h

SOLVER_SRCS = sat_solver.cpp 
SOLVER_OBJS = $(SOLVER_SRCS:.cpp=.o)

LIB_SRCS =  zchaff_utils.cpp zchaff_solver.cpp zchaff_base.cpp \
	    zchaff_dbase.cpp zchaff_c_wrapper.cpp zchaff_cpp_wrapper.cpp \
	    hash.cpp $(SOPT)

LIB_OBJS = $(LIB_SRCS:.cpp=.o)

cachet:	$(SOLVER_OBJS) libsat.a SAT_C.h
	$(CC) $(CFLAGS) $(SOLVER_OBJS) $(LFLAGS) libsat.a -o $(BINARY)  
	

$(LIB_OBJS): $(HEADERS) Makefile

libsat.a:   $(LIB_OBJS) 
	@rm -f libsat.a
	$(AR) cr libsat.a $(LIB_OBJS)
	$(RANLIB) libsat.a

.cpp.o:
	$(CC) $(CFLAGS) $(DFLAGS) $(MFLAGS)-c $< -o $*.o
.cc.o:
	$(CC) $(CFLAGS) $(MFLAGS)-c $< -o $*.o 

clean:	
	rm -f *.o libsat.a $(BINARY)
