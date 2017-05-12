# Path to the solver source code
SRC = ../../src/
# Path to the problem source code
PROB = ./

# Header and souce for the model
MODELHDR =	$(PROB)CorridorModel.h

MODELSRCS =	$(PROB)CorridorModel.cc

# Main file of solver
SOLVERMAIN = $(PROB)CorridorSolver.cc

# Main file of simulator
SIMULATORMAIN = $(PROB)CorridorSimulator.cc

# Main file of Controller
CONTROLLERMAIN = $(PROB)testController.cc

# Set compiler options for debugging etc.
#CXXFLAGS = -Wall -ggdb -O0
#CXXFLAGS = -Wall -ggdb -O0 -fopenmp
CXXFLAGS = -Wall -g -O3 -fopenmp

TARGETS = Solver Simulator Controller

include $(SRC)common.mk
