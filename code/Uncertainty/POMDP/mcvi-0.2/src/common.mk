# solver and problem directories
SRC ?= ../../src/
PROB ?= ./
BUILD ?= ../../build/

# AR
ARFLAGS = rvs

# No dupplicate names of source file
VPATH = $(SRC) $(PROB)

CXXFLAGS ?= -Wall -O2 -fopenmp

SOLVERMAIN ?= Solver.cc
SIMULATORMAIN ?= Simulator.cc
CONTROLLERMAIN ?= Controller.cc

# include directories
INCDIR = -I$(SRC) -I$(PROB)

SOLVEROBJ = $(SOLVERSRCS:$(SRC)%.cc=$(BUILD)%.o)
PROBOBJ = $(MODELSRCS:$(PROB)%.cc=%.o)
CONTROLLEROBJ = $(CONTROLLERSRCS:$(SRC)%.cc=$(BUILD)%.o)

SOLVERMAINOBJ = $(SOLVERMAIN:%.cc=%.o)
SIMULATORMAINOBJ = $(SIMULATORMAIN:%.cc=%.o)
CONTROLLERMAINOBJ = $(CONTROLLERMAIN:%.cc=%.o)

#HDRS = $(SOLVERHDR) $(MODELHDR) $(CONTROLLERHDR)
#SRCS = $(SOLVERSRCS) $(MODELSRCS) $(CONTROLLERSRCS)

# Header and souce for the Controller
CONTROLLERHDR ?= $(SRC)Controller.h
CONTROLLERSRCS ?= $(SRC)Controller.cc

SOLVERHDR = 	$(SRC)Action.h \
		$(SRC)Obs.h \
		$(SRC)ObsEdge.h \
		$(SRC)ActNode.h \
		$(SRC)Belief.h \
		$(SRC)ParticlesBelief.h \
		$(SRC)BeliefSet.h \
		$(SRC)ParticlesBeliefSet.h \
		$(SRC)BeliefTree.h \
		$(SRC)BeliefForest.h \
		$(SRC)Model.h \
		$(SRC)Bounds.h \
		$(SRC)PolicyGraph.h \
		$(SRC)RandSource.h \
		$(SRC)Simulator.h \
		$(SRC)ValueIteration.h \
		$(SRC)Solver.h

SOLVERSRCS =	$(SRC)Action.cc \
		$(SRC)Obs.cc \
		$(SRC)ObsEdge.cc \
		$(SRC)ActNode.cc \
		$(SRC)ParticlesBelief.cc \
		$(SRC)ParticlesBeliefSet.cc \
		$(SRC)BeliefTree.cc \
		$(SRC)BeliefForest.cc \
		$(SRC)Bounds.cc \
		$(SRC)PolicyGraph.cc \
		$(SRC)Simulator.cc \
		$(SRC)ValueIteration.cc \
		$(SRC)Solver.cc

# build into a library
TARGET_MCVI = $(BUILD)libmcvi.a
LIBS = -lmcvi

# files
TARGETS ?= $(TARGET_MCVI) Solver Simulator

# targets
.PHONY : all clean

all: $(TARGETS)

clean:
	rm -f *~ *.o *.obj *.a $(TARGETS) $(TARGET_MCVI) $(SOLVEROBJ) $(PROBOBJ) $(CONTROLLEROBJ)

$(TARGET_MCVI): $(SOLVEROBJ)
	$(AR) $(ARFLAGS) $@ $^

Solver: $(SOLVERMAINOBJ) $(PROBOBJ) $(TARGET_MCVI)
	$(LINK.cc) $< $(PROBOBJ) -L$(BUILD) $(LIBS) -o $@

Simulator: $(SIMULATORMAINOBJ) $(PROBOBJ) $(TARGET_MCVI)
	$(LINK.cc) $< $(PROBOBJ) -L$(BUILD) $(LIBS) -o $@

Controller: $(CONTROLLERMAINOBJ) $(CONTROLLEROBJ) $(PROBOBJ) $(TARGET_MCVI)
	$(LINK.cc) $< $(CONTROLLEROBJ) $(PROBOBJ) -L$(BUILD) $(LIBS) -o $@

# Automatic Dependency Generation
# see  http://mad-scientist.net/make/autodep.html

DEPDIR = .deps
df = $(DEPDIR)/$(*F)

#$(COMPILE.cc) -MMD $(INCDIR) -o $@ $<
$(BUILD)%.o : %.cc
	@mkdir -p $(BUILD)
	@mkdir -p $(BUILD)$(DEPDIR);
	$(COMPILE.cc) -MMD -o $@ $<
	@cp $(BUILD)$*.d $(BUILD)$(df).P; \
	 sed -e 's/#.*//' -e 's/^[^:]*: *//' -e 's/ *\\$$//' \
	 -e '/^$$/ d' -e 's/$$/ :/' < $(BUILD)$*.d >> $(BUILD)$(df).P; \
	 rm -f $(BUILD)$*.d

%.o : %.cc
	@mkdir -p $(DEPDIR);
	$(COMPILE.cc) -MMD $(INCDIR) -o $@ $<
	@cp $*.d $(df).P; \
	 sed -e 's/#.*//' -e 's/^[^:]*: *//' -e 's/ *\\$$//' \
	 -e '/^$$/ d' -e 's/$$/ :/' < $*.d >> $(df).P; \
	 rm -f $*.d

ALLSRCS = $(notdir $(SOLVERMAIN) $(SIMULATORMAIN) $(CONTROLLERMAIN) $(SRCS))

-include $(ALLSRCS:%.cc=$(BUILD)$(DEPDIR)/%.P)
-include $(ALLSRCS:%.cc=$(DEPDIR)/%.P)
