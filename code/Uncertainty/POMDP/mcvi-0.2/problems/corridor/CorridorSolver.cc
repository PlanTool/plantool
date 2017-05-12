#include "CorridorModel.h"
#include "Action.h"
#include "Solver.h"
#include "ParticlesBeliefSet.h"
#include "Include.h"
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

    Obs initialObs(vector<long>(currModel.getNumObsVar(), 0));
    initialObs.obs[0]=ObsNothing;

    solver.solve(currModel, initialObs);
}
