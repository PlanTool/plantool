SRC = ./
BUILD = ../build/

CXXFLAGS ?= -Wall -O2 -fopenmp

# include directories
INCDIR = -I$(SRC)

SOLVEROBJ = $(SOLVERSRCS:$(SRC)%.cc=$(BUILD)%.o)

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
TARGETS ?= $(TARGET_MCVI)

# targets
.PHONY : all clean

all: $(TARGETS)

clean:
	rm -f *~ *.o *.obj *.a $(TARGETS) $(SOLVEROBJ)

$(TARGET_MCVI): $(SOLVEROBJ)
	$(AR) $(ARFLAGS) $@ $^

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

-include $(ALLSRCS:%.cc=$(BUILD)$(DEPDIR)/%.P)
