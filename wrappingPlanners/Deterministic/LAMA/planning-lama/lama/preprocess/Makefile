TARGET = preprocess
HEADERS = helper_functions.h operator.h state.h successor_generator.h scc.h \
	  causal_graph.h max_dag.h variable.h domain_transition_graph.h \
	  axiom.h
SOURCES = planner.cc $(HEADERS:%.h=%.cc)
OBJECTS = $(SOURCES:%.cc=%.o)
PROFILE_OBJECTS = $(OBJECTS:%.o=%.profile.o)

PROFILE_TARGET = profile-$(TARGET)
PROFILE_PARAMETERS =

CC_OPTIONS = -ansi -Wall -O3 -Wno-sign-compare -pedantic -Werror
# CC_OPTIONS += -march=pentium3
PROFILE_OPTIONS = -pg -O3
LINK_OPTIONS =

CC = g++ -fPIC
DEPEND = g++ -MM
INC_PYTHON = -I/usr/include/python2.7

#vpath %.o obj

_preprocess.so:$(OBJECTS) preprocess_wrap.o
	g++  -fPIC -shared -o $@ $^
preprocess_wrap.o: preprocess_wrap.cxx
	g++ -c -fPIC $(INC_PYTHON) $^
reprocess.py preprocess_wrap.cxx: preprocess.i
	swig -c++ -python preprocess.i
	

$(TARGET): $(OBJECTS)
	$(CC) $(LINK_OPTIONS) $(OBJECTS:%=obj/%) -g -o $(TARGET)

$(PROFILE_TARGET): $(PROFILE_OBJECTS)
	$(CC) $(LINK_OPTIONS) $(PROFILE_OPTIONS) $(PROFILE_OBJECTS:%=obj/%) \
          -o $(PROFILE_TARGET)

PROFILE: $(PROFILE_TARGET)
	$(PROFILE_TARGET) $(PROFILE_PARAMETERS)
	gprof $(PROFILE_TARGET) > PROFILE

$(OBJECTS): %.o: %.cc
	$(CC) $(CC_OPTIONS) -c $< -o obj/$@

$(PROFILE_OBJECTS): %.profile.o: %.cc
	$(CC) $(CC_OPTIONS) $(PROFILE_OPTIONS) -c $< -o obj/$@

clean:
	rm -f $(OBJECTS:%=obj/%) $(PROFILE_OBJECTS:%=obj/%) *~
	rm -f Makefile.depend $(PROFILE_TARGET) gmon.out
	rm _preprocess.so
	rm preprocess_wrap.cxx preprocess.py preprocess.pyc preprocess_wrap.o

veryclean: clean
	rm -f $(TARGET) PROFILE

Makefile.depend: $(SOURCES) $(HEADERS)
	$(DEPEND) $(SOURCES) > Makefile.temp 
	sed -e "s/\(.*\)\.o:\(.*\)/\1.profile.o:\2/" Makefile.temp >> Makefile.temp2
	cat Makefile.temp Makefile.temp2 > Makefile.depend
	rm -f Makefile.temp Makefile.temp2

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),veryclean)
-include Makefile.depend
endif
endif
