#include "CorridorModel.h"
#include "Action.h"
#include "Solver.h"
#include "ParticlesBeliefSet.h"
#include "Obs.h"
#include <sstream>
#include <iostream>
#include <cstdlib>
#include <ctime>

using namespace std;

int main(int argc, char **argv)
{
    Solver solver;

    solver.input(argc, argv, 2);

    CorridorModel currModel(solver.numNextBeliefStreams);
    Obs initialObs(vector<long>(currModel.getNumObsVar(),0));
    initialObs.obs[1] = -1;

    solver.solve(currModel, initialObs);
}
