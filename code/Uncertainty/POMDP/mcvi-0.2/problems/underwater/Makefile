# Path to the solver source code
SRC = ../../src/
# Path to the problem source code
PROB = ./

# Header and souce for the model
MODELHDR =	$(PROB)UnderwaterModel.h

MODELSRCS =	$(PROB)UnderwaterModel.cc

# Main file of solver
SOLVERMAIN = $(PROB)UnderwaterSolver.cc

# Main file of simulator
SIMULATORMAIN = $(PROB)UnderwaterSimulator.cc

# Set compiler options for debugging etc.
#CXXFLAGS = -Wall -ggdb -O0
#CXXFLAGS = -Wall -ggdb -O0 -fopenmp
CXXFLAGS = -Wall -g -O3 -fopenmp

include $(SRC)common.mk
