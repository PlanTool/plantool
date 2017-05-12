# Path to the solver source code
SRC = ../../src/
# Path to the problem source code
PROB = ./

# Header and souce for the model
MODELHDR =	$(PROB)CorridorModel.h \
		$(PROB)Gauss.h \
		$(PROB)GaussMixture.h \
		$(PROB)GaussModel.h

MODELSRCS =	$(PROB)CorridorModel.cc \
		$(PROB)Gauss.cc \
		$(PROB)GaussMixture.cc \
		$(PROB)GaussModel.cc

# Main file of solver
SOLVERMAIN = $(PROB)CorridorSolver.cc

# Main file of simulator
SIMULATORMAIN = $(PROB)CorridorSimulator.cc

# Set compiler options for debugging etc.
#CXXFLAGS = -Wall -ggdb -O0
#CXXFLAGS = -Wall -ggdb -O0 -fopenmp
CXXFLAGS = -Wall -O3 -fopenmp

include $(SRC)common.mk
