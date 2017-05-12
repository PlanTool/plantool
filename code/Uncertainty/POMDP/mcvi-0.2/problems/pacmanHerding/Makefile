# Path to the solver source code
SRC = ../../src/
# Path to the problem source code
PROB = ./

# Header and souce for the model
MODELHDR =	$(PROB)Herding.h

MODELSRCS =	$(PROB)Herding.cc

# Main file of solver
SOLVERMAIN = $(PROB)HerdingSolver.cc

# Main file of simulator
SIMULATORMAIN = $(PROB)HerdingSimulator.cc

# Set compiler options for debugging etc.
#CXXFLAGS = -Wall -ggdb -O0
#CXXFLAGS = -Wall -O3
#CXXFLAGS = -Wall -ggdb -O0 -fopenmp
CXXFLAGS = -Wall -O3 -fopenmp

include $(SRC)common.mk
